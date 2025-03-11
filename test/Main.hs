{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception
import qualified Crypto.AEAD.ChaCha20Poly1305 as AEAD
import Data.ByteString as BS
import qualified Data.Aeson as A
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text.IO as TIO
import Data.Maybe (fromJust)
import Test.Tasty
import qualified Test.Tasty.HUnit as H
import qualified Wycheproof as W

main :: IO ()
main = do
  wycheproof_aead <- TIO.readFile "etc/chacha20_poly1305_test.json"
  let wycheproofs = A.decodeStrictText wycheproof_aead :: Maybe W.Wycheproof
  case wycheproofs of
    Nothing -> error "couldn't parse wycheproof vectors"
    Just w -> defaultMain $ testGroup "ppad-aead" [
        rfc8439
      , wycheproof_tests w
      ]

rfc8439 :: TestTree
rfc8439 = testGroup "RFC8439 vectors" [
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
    let nonce = salt <> iv

        (o_cip, o_tag) = AEAD.encrypt aad key nonce sunscreen

        e_cip = fromJust . B16.decode $
          "d31a8d34648e60db7b86afbc53ef7ec2a4aded51296e08fea9e2b5a736ee62d63dbea45e8ca9671282fafb69da92728b1a71de0a9e060b2905d6a5b67ecd3b3692ddbd7f2d778b8c9803aee328091b58fab324e4fad675945585808b4831d7bc3ff4def08e4b7a9de576d26586cec64b6116"

        e_tag = fromJust . B16.decode $
          "1ae10b594f09e26a7e902ecbd0600691"


        o_dec = AEAD.decrypt aad key nonce (o_cip, o_tag)

    H.assertEqual mempty (e_cip, e_tag) (o_cip, o_tag)
    H.assertEqual mempty (Just sunscreen) o_dec
  where
    sunscreen :: BS.ByteString
    sunscreen = fromJust . B16.decode $
      "4c616469657320616e642047656e746c656d656e206f662074686520636c617373206f66202739393a204966204920636f756c64206f6666657220796f75206f6e6c79206f6e652074697020666f7220746865206675747572652c2073756e73637265656e20776f756c642062652069742e"

    aad = fromJust . B16.decode $ "50515253c0c1c2c3c4c5c6c7"
    key = fromJust . B16.decode $
      "808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f"

    iv = fromJust . B16.decode $ "4041424344454647"
    salt = fromJust . B16.decode $ "07000000"

crypt0 :: TestTree
crypt0 = H.testCase "decrypt (A.5)" $ do
  let key = fromJust . B16.decode $
        "1c9240a5eb55d38af333888604f6b5f0473917c1402b80099dca5cbc207075c0"

      cip = fromJust . B16.decode $
        "64a0861575861af460f062c79be643bd5e805cfd345cf389f108670ac76c8cb24c6cfc18755d43eea09ee94e382d26b0bdb7b73c321b0100d4f03b7f355894cf332f830e710b97ce98c8a84abd0b948114ad176e008d33bd60f982b1ff37c8559797a06ef4f0ef61c186324e2b3506383606907b6a7c02b0f9f6157b53c867e4b9166c767b804d46a59b5216cde7a4e99040c5a40433225ee282a1b0a06c523eaf4534d7f83fa1155b0047718cbc546a0d072b04b3564eea1b422273f548271a0bb2316053fa76991955ebd63159434ecebb4e466dae5a1073a6727627097a1049e617d91d361094fa68f0ff77987130305beaba2eda04df997b714d6c6f2c29a6ad5cb4022b02709b"

      e_pan = fromJust . B16.decode $ "496e7465726e65742d4472616674732061726520647261667420646f63756d656e74732076616c696420666f722061206d6178696d756d206f6620736978206d6f6e74687320616e64206d617920626520757064617465642c207265706c616365642c206f72206f62736f6c65746564206279206f7468657220646f63756d656e747320617420616e792074696d652e20497420697320696e617070726f70726961746520746f2075736520496e7465726e65742d447261667473206173207265666572656e6365206d6174657269616c206f7220746f2063697465207468656d206f74686572207468616e206173202fe2809c776f726b20696e2070726f67726573732e2fe2809d"

      non = fromJust . B16.decode $
        "000000000102030405060708"

      aad = fromJust . B16.decode $
        "f33388860000000000004e91"

      tag = fromJust . B16.decode $
        "eead9d67890cbb22392336fea1851f38"

      Just pan = AEAD.decrypt aad key non (cip, tag)

  H.assertEqual mempty e_pan pan

wycheproof_tests :: W.Wycheproof -> TestTree
wycheproof_tests W.Wycheproof {..} =
  testGroup "wycheproof vectors (aead-chacha20-poly1305)" $
    fmap execute_group wp_testGroups

execute_group :: W.AEADTestGroup -> TestTree
execute_group W.AEADTestGroup {..} =
  testGroup mempty (fmap execute aeadtg_tests)

execute :: W.AEADTest -> TestTree
execute W.AEADTest {..} = H.testCase t_msg $ do
    let key = aeadt_key
        iv  = aeadt_iv
        aad = aeadt_aad
        msg = aeadt_msg
        ct  = aeadt_ct
        tag = aeadt_tag
    if   aeadt_result == "invalid"
    then do
      out <- try (pure $! AEAD.decrypt aad key iv (ct, tag))
               :: IO (Either ErrorCall (Maybe BS.ByteString))
      case out of
        Left _         -> H.assertBool "invalid (bogus key/nonce)" True
        Right Nothing  -> H.assertBool "invalid (bogus MAC)" True
        Right (Just o) -> H.assertBool "invalid" (msg /= o)
    else do
      let out = AEAD.decrypt aad key iv (ct, tag)
      H.assertEqual mempty (Just msg) out
  where
    t_msg = "test " <> show aeadt_tcId


