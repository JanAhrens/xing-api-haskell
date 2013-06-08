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
      pe_title       :: Maybe Text
    , pe_beginDate   :: Maybe Text
    , pe_endDate     :: Maybe Text
    , pe_careerLevel :: Maybe Text
    , pe_description :: Maybe Text
    , pe_name        :: Maybe Text
    , pe_tag         :: Maybe Text
    , pe_companySize :: Maybe Text
    , pe_url         :: Maybe Text
    , pe_industry    :: Text
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
