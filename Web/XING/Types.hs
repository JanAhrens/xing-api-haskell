{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Web.XING.Types
    (
      APIError(..)
    , RequestToken
    , Verifier
    , AccessToken
    , URL
    , User(..)
    , Gender(..)
    , UserId
    , PhotoUrls
    , Language, Skill
      -- * reexports
    , Manager
    , OAuth(..)
    , Credential(..)
    , BS.ByteString
    , Status
    ) where

import Network.HTTP.Conduit (Manager)
import qualified Data.ByteString as BS
import Web.Authenticate.OAuth (OAuth(..), Credential(..))
import Network.HTTP.Types (Status)
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Data.Text (Text)
import Data.Map (Map)
import Data.Aeson (FromJSON(..), Value(..))
import Control.Monad (mzero)

data APIError
  = OAuthError String
  | TokenError String
  | CallError String
  | Throttled
  | Mapping
  deriving (Eq, Show, Typeable)

instance Exception APIError

type RequestToken = Credential
type AccessToken  = Credential
type Verifier     = BS.ByteString
type URL          = BS.ByteString
type UserId       = Text
type PhotoUrls    = Map Text Text
type Language = Text
type Skill = Text

data Gender
  = Male
  | Female
  deriving (Eq, Show)

instance FromJSON Gender where
  parseJSON (String "m") = return Male
  parseJSON (String "f") = return Female
  parseJSON _            = mzero

class User a where
  userId      :: a -> UserId
  displayName :: a -> Text
  permalink   :: a -> Text
  photoUrls   :: a -> PhotoUrls
