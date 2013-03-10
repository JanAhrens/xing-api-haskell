{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Web.XING.Types
    (
      APIError(..)
    , RequestToken
    , Verifier
    , AccessToken
    , URL
      -- * reexports
    , Manager
    , OAuth
    , Credential
    , BS.ByteString
    , Status
    ) where

import Network.HTTP.Conduit (Manager)
import qualified Data.ByteString as BS
import Web.Authenticate.OAuth (OAuth, Credential)
import Network.HTTP.Types (Status)
import Control.Exception (Exception)
import Data.Typeable (Typeable)


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
