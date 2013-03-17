{-# LANGUAGE OverloadedStrings #-}

module Web.XING.Types.User(
    User(..)
  , FullUser
  , MinimalUser
) where

import Web.XING.Types.General
import Data.Aeson (FromJSON(..), Value(..), (.:))
import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)

data FullUser
  = FullUser
      UserId
      Text
      PhotoUrls
  deriving (Show, Eq)

data MinimalUser
  = MinimalUser
      UserId
      Text
      Text
      PhotoUrls
  deriving (Show, Eq)

class User a where
  userId      :: a -> UserId
  displayName :: a -> Text
  photoUrls   :: a -> PhotoUrls

instance User FullUser where
  userId (FullUser uid _ _)       = uid
  displayName (FullUser _ name _) = name
  photoUrls (FullUser _ _ urls)   = urls

instance User MinimalUser where
  userId (MinimalUser uid _ _ _ )      = uid
  displayName (MinimalUser _ name _ _) = name
  photoUrls (MinimalUser _ _ _ urls)   = urls

instance FromJSON FullUser where
  parseJSON (Object response) = do
    FullUser <$> (response .: "id")
             <*> (response .: "display_name")
             <*> (response .: "photo_urls")
  parseJSON _ = fail "no parse"

instance FromJSON MinimalUser where
  parseJSON (Object response) = do
    container <- response .: "id_card"
    MinimalUser <$> (container .: "id")
           <*> (container .: "display_name")
           <*> (container .: "permalink")
           <*> (container .: "photo_urls")
  parseJSON _ = fail "no parse"
