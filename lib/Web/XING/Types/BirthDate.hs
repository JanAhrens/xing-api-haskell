{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Web.XING.Types.BirthDate
    (
      BirthDate(..)
    ) where

import Data.Aeson (Value(..), FromJSON(..), (.:), (.:?))
import Control.Monad (mzero)

data BirthDate
  = FullDate Integer Int Int
  | DayOnly Int Int
  deriving (Eq, Show)

instance FromJSON BirthDate where
  parseJSON (Object response) = do
    maybeYear <- response .:? "year"
    month     <- response .:  "month"
    day       <- response .:  "day"
    case maybeYear of
      Just year -> return $ FullDate year month day
      _         -> return $ DayOnly day month
  parseJSON _ = mzero
