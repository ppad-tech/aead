{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Crypto.AEAD.ChaCha20Poly1305 as AEAD
import qualified Data.ByteString.Base16 as B16
import Test.Tasty
import qualified Test.Tasty.HUnit as H

main :: IO ()
main = defaultMain $ testGroup "ppad-aead" [
    poly1305_key_gen
  ]

poly1305_key_gen :: TestTree
poly1305_key_gen = H.testCase "poly1305_key_gen" $ do
  let Just key = B16.decode
        "808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f"
      Just non = B16.decode
        "000000000001020304050607"

      Just e = B16.decode
        "8ad5a08b905f81cc815040274ab29471a833b637e3fd0da508dbb8e2fdd1a646"

      o = AEAD.poly1305_key_gen key non
  H.assertEqual mempty e o

