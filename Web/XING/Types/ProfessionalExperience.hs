{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Web.XING.Types.ProfessionalExperience
    (
      ProfessionalExperience(..)
    ) where

import Data.Aeson (Value(..), FromJSON(..), (.:))
import Control.Monad (mzero)
import Control.Applicative ((<*>), (<$>))
import Data.Text (Text)

data ProfessionalExperience
  = ProfessionalExperience {
      title       :: Maybe Text
    , beginDate   :: Maybe Text
    , endDate     :: Maybe Text
    , careerLevel :: Maybe Text
    , description :: Maybe Text
    , name        :: Maybe Text
    , tag         :: Maybe Text
    , companySize :: Maybe Text
    , url         :: Maybe Text
    , industry    :: Text
  }
  deriving (Show, Eq)

instance FromJSON ProfessionalExperience where
  parseJSON (Object response) = do
    ProfessionalExperience <$> (response .: "title")
                           <*> (response .: "begin_date")
                           <*> (response .: "end_date")
                           <*> (response .: "career_level")
                           <*> (response .: "description")
                           <*> (response .: "name")
                           <*> (response .: "tag")
                           <*> (response .: "company_size")
                           <*> (response .: "url")
                           <*> (response .: "industry")
  parseJSON _ = mzero
