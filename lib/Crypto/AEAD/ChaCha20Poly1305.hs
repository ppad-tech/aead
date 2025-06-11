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

    -- * Error information
  , Error(..)

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
  -> Either Error BS.ByteString -- ^ 256-bit key (suitable for poly1305)
_poly1305_key_gen key nonce = case ChaCha20.block key 0 nonce of
  Left ChaCha20.InvalidKey -> Left InvalidKey
  Left ChaCha20.InvalidNonce -> Left InvalidNonce
  Right k -> pure (BS.take 32 k)
{-# INLINEABLE _poly1305_key_gen #-}

pad16 :: BS.ByteString -> BS.ByteString
pad16 (BI.PS _ _ l)
  | l `rem` 16 == 0 = mempty
  | otherwise = BS.replicate (16 - l `rem` 16) 0
{-# INLINE pad16 #-}

data Error =
    InvalidKey
  | InvalidNonce
  | InvalidMAC
  deriving (Eq, Show)

-- RFC8439 2.8

-- | Perform authenticated encryption on a plaintext and some additional
--   authenticated data, given a 256-bit key and 96-bit nonce, using
--   AEAD-ChaCha20-Poly1305.
--
--   Produces a ciphertext and 128-bit message authentication code pair.
--
--   >>> let key = "don't tell anyone my secret key!"
--   >>> let non = "or my nonce!"
--   >>> let pan = "and here's my plaintext"
--   >>> let aad = "i approve this message"
--   >>> let Right (cip, mac) = encrypt aad key nonce pan
--   >>> (cip, mac)
--   <(ciphertext, 128-bit MAC)>
encrypt
  :: BS.ByteString -- ^ arbitrary-length additional authenticated data
  -> BS.ByteString -- ^ 256-bit key
  -> BS.ByteString -- ^ 96-bit nonce
  -> BS.ByteString -- ^ arbitrary-length plaintext
  -> Either Error (BS.ByteString, BS.ByteString) -- ^ (ciphertext, 128-bit MAC)
encrypt aad key nonce plaintext
  | BS.length key  /= 32  = Left InvalidKey
  | BS.length nonce /= 12 = Left InvalidNonce
  | otherwise = do
      otk <- _poly1305_key_gen key nonce
      case ChaCha20.cipher key 1 nonce plaintext of
        Left ChaCha20.InvalidKey -> Left InvalidKey     -- impossible, but..
        Left ChaCha20.InvalidNonce -> Left InvalidNonce -- ditto
        Right cip -> do
          let md0 = aad <> pad16 aad
              md1 = md0 <> cip <> pad16 cip
              md2 = md1 <> unroll8 (fi (BS.length aad))
              md3 = md2 <> unroll8 (fi (BS.length cip))
          case Poly1305.mac otk md3 of
            Nothing -> Left InvalidKey
            Just tag -> pure (cip, tag)

-- | Decrypt an authenticated ciphertext, given a message authentication
--   code and some additional authenticated data, via a 256-bit key and
--   96-bit nonce.
--
--   >>> decrypt aad key non (cip, mac)
--   Right "and here's my plaintext"
--   >>> decrypt aad key non (cip, "it's a valid mac")
--   Left InvalidMAC
decrypt
  :: BS.ByteString                  -- ^ arbitrary-length AAD
  -> BS.ByteString                  -- ^ 256-bit key
  -> BS.ByteString                  -- ^ 96-bit nonce
  -> (BS.ByteString, BS.ByteString) -- ^ (arbitrary-length ciphertext, 128-bit MAC)
  -> Either Error BS.ByteString
decrypt aad key nonce (cip, mac)
  | BS.length key /= 32   = Left InvalidKey
  | BS.length nonce /= 12 = Left InvalidNonce
  | BS.length mac /= 16   = Left InvalidMAC
  | otherwise = do
      otk <- _poly1305_key_gen key nonce
      let md0 = aad <> pad16 aad
          md1 = md0 <> cip <> pad16 cip
          md2 = md1 <> unroll8 (fi (BS.length aad))
          md3 = md2 <> unroll8 (fi (BS.length cip))
      case Poly1305.mac otk md3 of
        Nothing -> Left InvalidKey
        Just tag
          | mac == tag -> case ChaCha20.cipher key 1 nonce cip of
              Left ChaCha20.InvalidKey -> Left InvalidKey
              Left ChaCha20.InvalidNonce -> Left InvalidNonce
              Right v -> pure v
          | otherwise ->
              Left InvalidMAC

