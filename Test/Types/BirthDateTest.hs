{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Types.BirthDateTest where

import Test.Framework
import Web.XING.Types.BirthDate
import Data.Maybe
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL

-- Valid BirthDate

test_fullBirthDateCorrectMapped :: IO ()
test_fullBirthDateCorrectMapped
  = assertEqual (FullDate 1963 8 12) (fromJust $ parseBirthDate fullBirthDate)
  where
    fullBirthDate =
      "  {                  \
      \    \"day\":     12, \
      \    \"month\":    8, \
      \    \"year\":  1963  \
      \  }                  "

test_dayOnlyCorrectMapped :: IO ()
test_dayOnlyCorrectMapped
  = assertEqual (DayOnly 12 8) (fromJust $ parseBirthDate dayOnly)
  where
    dayOnly =
      "  {                  \
      \    \"day\":     12, \
      \    \"month\":    8, \
      \    \"year\":  null  \
      \  }                  "

-- Invalid BirthDate

test_emptyHash :: IO ()
test_emptyHash
  = assertInvalidBirthDate "{}"

test_nullValuesAreNotAValidBirthDate :: IO ()
test_nullValuesAreNotAValidBirthDate
  = assertInvalidBirthDate
      "  {                  \
      \    \"day\":   null, \
      \    \"month\": null, \
      \    \"year\":  null  \
      \  }                  "

test_incompleteHash :: IO ()
test_incompleteHash
  = assertInvalidBirthDate
      "  {                  \
      \    \"day\":     12, \
      \    \"year\":  1963  \
      \  }                  "

parseBirthDate
  :: BSL.ByteString
  -> Maybe BirthDate
parseBirthDate = decode

assertInvalidBirthDate
  :: BSL.ByteString
  -> IO ()
assertInvalidBirthDate = assertBool . isNothing . parseBirthDate

main :: IO ()
main = htfMain htf_thisModulesTests
