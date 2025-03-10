{-# LANGUAGE OverloadedStrings #-}

module Crypto.AEAD.ChaCha20Poly1305 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI
import qualified Crypto.Cipher.ChaCha20 as ChaCha20
import qualified Crypto.MAC.Poly1305 as Poly1305

poly1305_key_gen
  :: BS.ByteString -- ^ 256-bit initial keying material
  -> BS.ByteString -- ^ 96-bit nonce
  -> BS.ByteString -- ^ 256-bit key (suitable for poly1305)
poly1305_key_gen key@(BI.PS _ _ l) nonce
  | l /= 32   = error "ppad-aead (poly1305_key_gen): invalid key"
  | otherwise = BS.take 32 (ChaCha20.block key 0 nonce)
