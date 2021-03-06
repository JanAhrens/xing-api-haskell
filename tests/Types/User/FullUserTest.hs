{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.User.FullUserTest where

import Test.Framework
import Web.XING
import Data.Maybe
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL

test_noContentValid :: IO ()
test_noContentValid
  = (assertBool . isJust . parseFullUser) fullUserWithoutContent

-- https://dev.xing.com/docs/user_profile
fullUserWithoutContent :: BSL.ByteString
fullUserWithoutContent =
  "  {                                \
  \    \"id\":           \"\",        \
  \    \"display_name\": \"\",        \
  \    \"permalink\":    \"\",        \
  \    \"first_name\":   \"\",        \
  \    \"last_name\":    \"\",        \
  \    \"page_name\":    \"\",        \
  \    \"gender\":       \"m\",       \
  \    \"active_email\": null,        \
  \    \"time_zone\": {               \
  \      \"utc_offset\": 2.0,         \
  \      \"name\": \"Europe/Berlin\"  \
  \    },                             \
  \    \"premium_services\": [],      \
  \    \"badges\":           [],      \
  \    \"languages\": {},             \
  \    \"wants\": \"\",               \
  \    \"haves\": null,               \
  \    \"interests\": null,           \
  \    \"organisation_member\": null, \
  \    \"private_address\": {         \
  \      \"street\":       \"\",      \
  \      \"zip_code\":     \"\",      \
  \      \"city\":         \"\",      \
  \      \"province\":     \"\",      \
  \      \"country\":      \"\",      \
  \      \"email\":        \"\",      \
  \      \"phone\":        \"\",      \
  \      \"fax\":          \"\",      \
  \      \"mobile_phone\": \"\"       \
  \    },                             \
  \    \"business_address\": {        \
  \      \"street\":       \"\",      \
  \      \"zip_code\":     \"\",      \
  \      \"city\":         \"\",      \
  \      \"province\":     \"\",      \
  \      \"country\":      \"\",      \
  \      \"email\":        \"\",      \
  \      \"phone\":        \"\",      \
  \      \"fax\":          \"\",      \
  \      \"mobile_phone\": \"\"       \
  \    },                             \
  \    \"professional_experience\": {     \
  \      \"primary_company\": {           \
  \        \"title\":        null,        \
  \        \"begin_date\":   null,        \
  \        \"end_date\":     null,        \
  \        \"career_level\": null,        \
  \        \"description\":  null,        \
  \                                       \
  \        \"name\":         null,        \
  \        \"tag\":          null,        \
  \        \"company_size\": null,        \
  \        \"url\":          null,        \
  \        \"industry\":     \"OTHERS\"   \
  \      },                               \
  \      \"non_primary_companies\": [],   \
  \      \"awards\": []                   \
  \    },                                 \
  \    \"photo_urls\": {},            \
  \    \"birth_date\": {}             \
  \  }                                "

parseFullUser
  :: BSL.ByteString
  -> Maybe FullUser
parseFullUser = decode

main :: IO ()
main = htfMain htf_thisModulesTests
