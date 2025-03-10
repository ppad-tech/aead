{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Crypto.AEAD.ChaCha20Poly1305 as AEAD
import Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Maybe (fromJust)
import Test.Tasty
import qualified Test.Tasty.HUnit as H

main :: IO ()
main = defaultMain $ testGroup "ppad-aead" [
    poly1305_key_gen
  , crypt
  ]

poly1305_key_gen :: TestTree
poly1305_key_gen = H.testCase "poly1305_key_gen" $ do
  let Just key = B16.decode
        "808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f"
      Just non = B16.decode
        "000000000001020304050607"

      Just e = B16.decode
        "8ad5a08b905f81cc815040274ab29471a833b637e3fd0da508dbb8e2fdd1a646"

      o = AEAD._poly1305_key_gen key non
  H.assertEqual mempty e o

crypt :: TestTree
crypt = H.testCase "encrypt/decrypt" $ do
    let (o_cip, o_tag) = AEAD.encrypt aad key iv salt sunscreen

        e_cip = fromJust . B16.decode $
          "d31a8d34648e60db7b86afbc53ef7ec2a4aded51296e08fea9e2b5a736ee62d63dbea45e8ca9671282fafb69da92728b1a71de0a9e060b2905d6a5b67ecd3b3692ddbd7f2d778b8c9803aee328091b58fab324e4fad675945585808b4831d7bc3ff4def08e4b7a9de576d26586cec64b6116"

        e_tag = fromJust . B16.decode $
          "1ae10b594f09e26a7e902ecbd0600691"

        o_dec = AEAD.decrypt aad key iv salt (o_cip, o_tag)
        o_fal = AEAD.decrypt aad key iv salt (o_cip, BS.replicate 16 0)

    H.assertEqual mempty (e_cip, e_tag) (o_cip, o_tag)
    H.assertEqual mempty (Right sunscreen) o_dec
    H.assertEqual mempty (Left sunscreen) o_fal
  where
    sunscreen :: BS.ByteString
    sunscreen = fromJust . B16.decode $
      "4c616469657320616e642047656e746c656d656e206f662074686520636c617373206f66202739393a204966204920636f756c64206f6666657220796f75206f6e6c79206f6e652074697020666f7220746865206675747572652c2073756e73637265656e20776f756c642062652069742e"

    aad = fromJust . B16.decode $ "50515253c0c1c2c3c4c5c6c7"
    key = fromJust . B16.decode $
      "808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f"

    iv = fromJust . B16.decode $ "4041424344454647"
    salt = fromJust . B16.decode $ "07000000"

