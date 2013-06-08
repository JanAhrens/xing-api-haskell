{-# LANGUAGE OverloadedStrings #-}

module Web.XING.Types.Award
    (
      Award(..)
    ) where

import Data.Aeson (Value(..), FromJSON(..), (.:), (.:?))
import Control.Monad (mzero)
import Control.Applicative ((<*>), (<$>))
import Data.Text (Text)

data Award
  = Award Text Int (Maybe Text)
  deriving (Show, Eq)

instance FromJSON Award where
  parseJSON (Object response) = do
    Award <$> (response .: "name")
          <*> (response .: "date_awarded")
          <*> (response .:? "url")
  parseJSON _ = mzero
