{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Types.AwardTest where

import Test.Framework
import Web.XING.Types.Award
import Data.Maybe
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL

-- valid fields

noContentAward :: BSL.ByteString
noContentAward
  = "  {                          \
    \     \"name\":         \"\", \
    \     \"date_awarded\": 2007, \
    \     \"url\":          null  \
    \  }                          "

test_noContent :: IO ()
test_noContent
  = assertValidAward noContentAward

test_noContentMappedCorrect :: IO ()
test_noContentMappedCorrect
  = assertEqual (Award "" 2007 Nothing) (fromJust.parseAward $ noContentAward)

test_fullAwardMappedCorrect :: IO ()
test_fullAwardMappedCorrect
  = assertEqual (Award "Spacewalker" 1998 (Just "http://nasa.gov")) ((fromJust.parseAward)
      "  {                                         \
      \     \"name\":         \"Spacewalker\",     \
      \     \"url\":          \"http://nasa.gov\", \
      \     \"date_awarded\": 1998                 \
      \  }                                         ")

parseAward
  :: BSL.ByteString
  -> Maybe Award
parseAward = decode

assertValidAward
  :: BSL.ByteString
  -> IO ()
assertValidAward = assertBool . isJust . parseAward
