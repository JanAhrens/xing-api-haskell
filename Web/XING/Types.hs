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
    , UserId
    , PhotoUrls
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

class User a where
  userId      :: a -> UserId
  displayName :: a -> Text
  photoUrls   :: a -> PhotoUrls
