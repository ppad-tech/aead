{-# LANGUAGE OverloadedStrings #-}

module Wycheproof (
    Wycheproof(..)
  , AEADTestGroup(..)
  , AEADTest(..)
  ) where

import Data.Aeson ((.:))
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

data Wycheproof = Wycheproof {
    wp_numberOfTests :: !Int
  , wp_testGroups    :: ![AEADTestGroup]
  } deriving Show

instance A.FromJSON Wycheproof where
  parseJSON = A.withObject "Wycheproof" $ \m -> Wycheproof
    <$> m .: "numberOfTests"
    <*> m .: "testGroups"

data AEADTestGroup = AEADTestGroup {
    aeadtg_type    :: !T.Text
  , aeadtg_ivSize  :: !Int
  , aeadtg_keySize :: !Int
  , aeadtg_tagSize :: !Int
  , aeadtg_tests   :: ![AEADTest]
  } deriving Show

instance A.FromJSON AEADTestGroup where
  parseJSON = A.withObject "AEADTestGroup" $ \m -> AEADTestGroup
    <$> m .: "type"
    <*> m .: "ivSize"
    <*> m .: "keySize"
    <*> m .: "tagSize"
    <*> m .: "tests"

data AEADTest = AEADTest {
    aeadt_tcId           :: !Int
  , aeadt_comment        :: !T.Text
  , aeadt_key            :: !BS.ByteString
  , aeadt_iv             :: !BS.ByteString
  , aeadt_aad            :: !BS.ByteString
  , aeadt_msg            :: !BS.ByteString
  , aeadt_ct             :: !BS.ByteString
  , aeadt_tag            :: !BS.ByteString
  , aeadt_result         :: !T.Text
  } deriving Show

decodehex :: T.Text -> BS.ByteString
decodehex t = case B16.decode (TE.encodeUtf8 t) of
  Nothing -> error "bang"
  Just bs -> bs

instance A.FromJSON AEADTest where
  parseJSON = A.withObject "AEADTest" $ \m -> AEADTest
    <$> m .: "tcId"
    <*> m .: "comment"
    <*> fmap decodehex (m .: "key")
    <*> fmap decodehex (m .: "iv")
    <*> fmap decodehex (m .: "aad")
    <*> fmap decodehex (m .: "msg")
    <*> fmap decodehex (m .: "ct")
    <*> fmap decodehex (m .: "tag")
    <*> m .: "result"

