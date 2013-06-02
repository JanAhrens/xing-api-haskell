{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Types.AddressTest where

import Test.Framework
import Web.XING.Types.Address
import Data.Maybe
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Text.Lazy.Encoding (encodeUtf8)

-- valid fields

test_noContent :: IO ()
test_noContent
  = assertValidAddress
      "  {                           \
      \     \"street\":       \"\",  \
      \     \"zip_code\":     \"\",  \
      \     \"city\":         \"\",  \
      \     \"province\":     \"\",  \
      \     \"country\":      \"\",  \
      \     \"email\":        \"\",  \
      \     \"phone\":        \"\",  \
      \     \"fax\":          \"\",  \
      \     \"mobile_phone\": \"\"   \
      \  }                           "

test_nullValuesAreValid :: IO ()
test_nullValuesAreValid
  = assertValidAddress
      "  {                           \
      \     \"street\":       null,  \
      \     \"zip_code\":     null,  \
      \     \"city\":         null,  \
      \     \"province\":     null,  \
      \     \"country\":      null,  \
      \     \"email\":        null,  \
      \     \"phone\":        null,  \
      \     \"fax\":          null,  \
      \     \"mobile_phone\": null   \
      \  }                           "

test_fullAddress :: IO ()
test_fullAddress
  = assertValidAddress . encodeUtf8 $
      "  {                                           \
      \     \"street\":       \"Privatstraße 1\",    \
      \     \"zip_code\":     \"20357\",             \
      \     \"city\":         \"Hamburg\",           \
      \     \"province\":     \"Hamburg\",           \
      \     \"country\":      \"DE\",                \
      \     \"email\":        \"max@mustermann.de\", \
      \     \"phone\":        \"49|40|1234560\",     \
      \     \"fax\":          \"||\",                \
      \     \"mobile_phone\": \"49|0155|1234567\"    \
      \  }                                           "

-- invalid fields

test_emptyHashNotValid :: IO ()
test_emptyHashNotValid
  = assertInvalidAddress
      "{}"

test_someFieldsMissing :: IO ()
test_someFieldsMissing
  = assertInvalidAddress
      "  {                           \
      \     \"street\":       \"\",  \
      \     \"zip_code\":     \"\",  \
      \     \"city\":         \"\",  \
      \     \"province\":     \"\",  \
      \     \"country\":      \"\",  \
      \     \"email\":        \"\"   \
      \  }                           "

parseAddress
  :: BSL.ByteString
  -> Maybe Address
parseAddress = decode

assertValidAddress, assertInvalidAddress
  :: BSL.ByteString
  -> IO ()
assertValidAddress   = assertBool . isJust    . parseAddress
assertInvalidAddress = assertBool . isNothing . parseAddress
