{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Crypto.AEAD.ChaCha20Poly1305
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- A pure AEAD-ChaCha20-Poly1305 implementation, as specified by
-- [RFC 8439](https://datatracker.ietf.org/doc/html/rfc8439).

module Crypto.AEAD.ChaCha20Poly1305 (
    -- * AEAD construction
    encrypt
  , decrypt

    -- testing
  , _poly1305_key_gen
  ) where

import qualified Crypto.Cipher.ChaCha20 as ChaCha20
import qualified Crypto.MAC.Poly1305 as Poly1305
import Data.Bits ((.>>.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI
import Data.Word (Word64)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

-- little-endian bytestring encoding
unroll :: Word64 -> BS.ByteString
unroll i = case i of
    0 -> BS.singleton 0
    _ -> BS.unfoldr coalg i
  where
    coalg = \case
      0 -> Nothing
      m -> Just $! (fi m, m .>>. 8)
{-# INLINE unroll #-}

-- little-endian bytestring encoding for 64-bit ints, right-padding with zeros
unroll8 :: Word64 -> BS.ByteString
unroll8 (unroll -> u@(BI.PS _ _ l))
  | l < 8 = u <> BS.replicate (8 - l) 0
  | otherwise = u
{-# INLINE unroll8 #-}

-- RFC8439 2.6

_poly1305_key_gen
  :: BS.ByteString -- ^ 256-bit initial keying material
  -> BS.ByteString -- ^ 96-bit nonce
  -> BS.ByteString -- ^ 256-bit key (suitable for poly1305)
_poly1305_key_gen key@(BI.PS _ _ l) nonce
  | l /= 32   = error "ppad-aead (poly1305_key_gen): invalid key"
  | otherwise = BS.take 32 (ChaCha20.block key 0 nonce)
{-# INLINEABLE _poly1305_key_gen #-}

pad16 :: BS.ByteString -> BS.ByteString
pad16 (BI.PS _ _ l)
  | l == 16   = mempty
  | otherwise = BS.replicate (16 - l `rem` 16) 0
{-# INLINE pad16 #-}

-- RFC8439 2.8

-- | Perform authenticated encryption on a plaintext and some additional
--   authenticated data, given a 256-bit key and 96-bit nonce, using
--   AEAD-ChaCha20-Poly1305.
--
--   Produces a ciphertext and 128-bit message authentication code pair.
--
--   Providing an invalid key or nonce will result in an 'ErrorCall'
--   exception being thrown.
--
--   >>> let key = "don't tell anyone my secret key!"
--   >>> let non = "or my nonce!"
--   >>> let pan = "and here's my plaintext"
--   >>> let aad = "i approve this message"
--   >>> let (cip, mac) = encrypt aad key nonce pan
--   >>> (cip, mac)
--   <(ciphertext, 128-bit MAC)>
encrypt
  :: BS.ByteString -- ^ arbitrary-length additional authenticated data
  -> BS.ByteString -- ^ 256-bit key
  -> BS.ByteString -- ^ 96-bit nonce
  -> BS.ByteString -- ^ arbitrary-length plaintext
  -> (BS.ByteString, BS.ByteString) -- ^ (ciphertext, 128-bit MAC)
encrypt aad key nonce plaintext
  | BS.length key  /= 32  = error "ppad-aead (encrypt): invalid key"
  | BS.length nonce /= 12 = error "ppad-aead (encrypt): invalid nonce"
  | otherwise =
      let otk = _poly1305_key_gen key nonce
          cip = ChaCha20.cipher key 1 nonce plaintext
          md0 | BS.length aad == 0 = mempty
              | otherwise          = aad <> pad16 aad
          md1 | BS.length cip == 0 = md0
              | otherwise          = md0 <> cip <> pad16 cip
          md2 = md1 <> unroll8 (fi (BS.length aad))
          md3 = md2 <> unroll8 (fi (BS.length cip))
          tag = Poly1305.mac otk md3
      in  (cip, tag)

-- | Decrypt an authenticated ciphertext, given a message authentication
--   code and some additional authenticated data, via a 256-bit key and
--   96-bit nonce.
--
--   Returns 'Nothing' if the MAC fails to validate.
--
--   Providing an invalid key or nonce will result in an 'ErrorCall'
--   exception being thrown.
--
--   >>> decrypt aad key non (cip, mac)
--   Just "and here's my plaintext"
--   >>> decrypt aad key non (cip, "it's a valid mac")
--   Nothing
decrypt
  :: BS.ByteString                  -- ^ arbitrary-length AAD
  -> BS.ByteString                  -- ^ 256-bit key
  -> BS.ByteString                  -- ^ 96-bit nonce
  -> (BS.ByteString, BS.ByteString) -- ^ (arbitrary-length ciphertext, 128-bit MAC)
  -> Maybe BS.ByteString
decrypt aad key nonce (cip, mac)
  | BS.length key /= 32   = error "ppad-aead (decrypt): invalid key"
  | BS.length nonce /= 12 = error "ppad-aead (decrypt): invalid nonce"
  | BS.length mac /= 16   = Nothing
  | otherwise =
      let otk = _poly1305_key_gen key nonce
          md0 | BS.length aad == 0 = mempty
              | otherwise          = aad <> pad16 aad
          md1 | BS.length cip == 0 = md0
              | otherwise          = md0 <> cip <> pad16 cip
          md2 = md1 <> unroll8 (fi (BS.length aad))
          md3 = md2 <> unroll8 (fi (BS.length cip))
          tag = Poly1305.mac otk md3
      in  if   mac == tag
          then pure (ChaCha20.cipher key 1 nonce cip)
          else Nothing

