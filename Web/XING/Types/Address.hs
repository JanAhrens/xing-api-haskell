{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Web.XING.Types.Address
    (
      Address(..)
    ) where

import Data.Aeson (Value(..), FromJSON(..), (.:))
import Control.Monad (mzero)
import Control.Applicative ((<*>), (<$>))
import Data.Text (Text)

-- the address model is pretty terrible -  nothing is certain ..
data Address
  = Address {
      street      :: Maybe Text
    , zipCode     :: Maybe Text
    , city        :: Maybe Text
    , province    :: Maybe Text
    , country     :: Maybe Text
    , email       :: Maybe Text
    , phone       :: Maybe Text
    , fax         :: Maybe Text
    , mobilePhone :: Maybe Text
  }
  deriving (Show, Eq)

instance FromJSON Address where
  parseJSON (Object response) = do
    Address <$> (response .: "street")
            <*> (response .: "zip_code")
            <*> (response .: "city")
            <*> (response .: "province")
            <*> (response .: "country")
            <*> (response .: "email")
            <*> (response .: "phone")
            <*> (response .: "fax")
            <*> (response .: "mobile_phone")
  parseJSON _ = mzero
