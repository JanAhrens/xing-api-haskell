{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.ProfessionalExperienceTest where

import Test.Framework
import Web.XING
import Data.Maybe
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL

-- valid fields

test_noContentValid :: IO ()
test_noContentValid
  = assertValidProfessionalExperience
      "  {                              \
      \    \"title\":        null,      \
      \    \"begin_date\":   null,      \
      \    \"end_date\":     null,      \
      \    \"career_level\": null,      \
      \    \"description\":  null,      \
      \    \"name\":         null,      \
      \    \"tag\":          null,      \
      \    \"company_size\": null,      \
      \    \"url\":          null,      \
      \    \"industry\":     \"OTHERS\" \
      \  }                              "

fullProfessionalExperience :: BSL.ByteString
fullProfessionalExperience =
      "  {                                                 \
      \    \"title\":        \"Softwareentwickler\",       \
      \    \"begin_date\":   \"2010-01\",                  \
      \    \"end_date\":     \"2012-12\",                  \
      \    \"career_level\": \"PROFESSIONAL_EXPERIENCED\", \
      \    \"description\":  \"awesome description\",      \
      \    \"name\":         \"XING AG\",                  \
      \    \"tag\":          \"XINGAG\",                   \
      \    \"company_size\": \"201-500\",                  \
      \    \"url\":          \"https://www.xing.com\",     \
      \    \"industry\":     \"OTHERS\"                    \
      \  }                                                 "

test_fullProfessionalExperienceValid :: IO ()
test_fullProfessionalExperienceValid
  = assertValidProfessionalExperience fullProfessionalExperience

test_contentMappedCorrect :: IO ()
test_contentMappedCorrect
  = assertEqual (ProfessionalExperience
      (Just "Softwareentwickler")
      (Just "2010-01")
      (Just "2012-12")
      (Just "PROFESSIONAL_EXPERIENCED")
      (Just "awesome description")
      (Just "XING AG")
      (Just "XINGAG")
      (Just "201-500")
      (Just "https://www.xing.com")
      "OTHERS") (fromJust $ parseProfessionalExperience fullProfessionalExperience)

-- invalid fields

test_emptyHashNotValid :: IO ()
test_emptyHashNotValid
  = assertInvalidProfessionalExperience
      "{}"

test_someFieldsMissing :: IO ()
test_someFieldsMissing
  = assertInvalidProfessionalExperience
      "  {                              \
      \    \"name\":         null,      \
      \    \"tag\":          null,      \
      \    \"company_size\": null,      \
      \    \"url\":          null,      \
      \    \"industry\":     \"OTHERS\" \
      \  }                              "

parseProfessionalExperience
  :: BSL.ByteString
  -> Maybe ProfessionalExperience
parseProfessionalExperience = decode

assertValidProfessionalExperience, assertInvalidProfessionalExperience
  :: BSL.ByteString
  -> IO ()
assertValidProfessionalExperience   = assertBool . isJust    . parseProfessionalExperience
assertInvalidProfessionalExperience = assertBool . isNothing . parseProfessionalExperience

main :: IO ()
main = htfMain htf_thisModulesTests
