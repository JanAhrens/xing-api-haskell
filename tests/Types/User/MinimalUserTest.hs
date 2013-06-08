{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.User.MinimalUserTest where

import Test.Framework
import Web.XING
import Data.Maybe
import Data.Map
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL

test_noContentValid :: IO ()
test_noContentValid
  = (assertBool . isJust . parseMinimalUser) minimalUserWithoutContent

test_minimalUserCorrectMapped :: IO ()
test_minimalUserCorrectMapped
  = assertEqual (MinimalUser "" "" "" empty) (fromJust $ parseMinimalUser minimalUserWithoutContent)

minimalUserWithoutContent :: BSL.ByteString
minimalUserWithoutContent =
  "  {                           \
  \    \"id_card\": {            \
  \      \"id\":           \"\", \
  \      \"display_name\": \"\", \
  \      \"permalink\":    \"\", \
  \      \"photo_urls\":   {}    \
  \    }                         \
  \  }                           "

parseMinimalUser
  :: BSL.ByteString
  -> Maybe MinimalUser
parseMinimalUser = decode

main :: IO ()
main = htfMain htf_thisModulesTests
