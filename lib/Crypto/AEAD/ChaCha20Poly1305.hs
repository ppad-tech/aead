{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

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
pad16 (BI.PS _ _ l) = BS.replicate (16 - l `rem` 16) 0
{-# INLINE pad16 #-}

-- RFC8439 2.8

encrypt
  :: BS.ByteString -- ^ arbitrary-length additional authenticated data
  -> BS.ByteString -- ^ 256-bit key
  -> BS.ByteString -- ^ 64-bit initial value (IV)
  -> BS.ByteString -- ^ 32-bit salt
  -> BS.ByteString -- ^ arbitrary-length plaintext
  -> (BS.ByteString, BS.ByteString) -- ^ (ciphertext, 128-bit MAC)
encrypt aad key iv salt plaintext
  | BS.length key  /= 32 = error "ppad-aead (encrypt): invalid key"
  | BS.length iv   /= 8  = error "ppad-aead (encrypt): invalid IV"
  | BS.length salt /= 4  = error "ppad-aead (encrypt): invalid salt"
  | otherwise =
      let nonce = salt <> iv
          otk   = _poly1305_key_gen key nonce
          ciphertext = ChaCha20.cipher key 1 nonce plaintext
          md0 = aad <> pad16 aad
          md1 = md0 <> ciphertext <> pad16 ciphertext
          md2 = md1 <> unroll8 (fi (BS.length aad))
          md3 = md2 <> unroll8 (fi (BS.length ciphertext))
          tag = Poly1305.mac otk md3
      in  (ciphertext, tag)

decrypt
  :: BS.ByteString -- ^ arbitrary-length additional authenticated data
  -> BS.ByteString -- ^ 256-bit key
  -> BS.ByteString -- ^ 64-bit initial value (IV)
  -> BS.ByteString -- ^ 32-bit salt
  -> (BS.ByteString, BS.ByteString) -- ^ (arbitrary-length ciphertext, 128-bit MAC)
  -> Either BS.ByteString BS.ByteString -- ^ possibly-authenticated plaintext
decrypt aad key iv salt (ciphertext, mac) =
  let (plaintext, _) = encrypt aad key iv salt ciphertext
      (_, tag) = encrypt aad key iv salt plaintext  -- XX seems wrong?
  in  if   mac == tag
      then Right plaintext
      else Left plaintext

