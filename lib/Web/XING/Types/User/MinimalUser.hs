{-# LANGUAGE OverloadedStrings #-}

module Web.XING.Types.User.MinimalUser
    (
      MinimalUser(..)
    ) where

import Web.XING.Types.User
import Data.Aeson (Value(..), FromJSON(..), (.:))
import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)

data MinimalUser
  = MinimalUser
      UserId
      Text
      Text
      PhotoUrls
  deriving (Show, Eq)

instance User MinimalUser where
  userId (MinimalUser uid _ _ _ )      = uid
  displayName (MinimalUser _ name _ _) = name
  permalink (MinimalUser _ _ link _)   = link
  photoUrls (MinimalUser _ _ _ urls)   = urls

instance FromJSON MinimalUser where
  parseJSON (Object response) = do
    container <- response .: "id_card"
    MinimalUser <$> (container .: "id")
           <*> (container .: "display_name")
           <*> (container .: "permalink")
           <*> (container .: "photo_urls")
  parseJSON _ = fail "no parse"
