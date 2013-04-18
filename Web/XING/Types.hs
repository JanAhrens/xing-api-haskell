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
    , OAuth(..)
    , Credential(..)
    , BS.ByteString
    , Status
    , module Web.XING.Types.BirthDate
    , module Web.XING.Types.User
    , module Web.XING.Types.User.FullUser
    , module Web.XING.Types.User.MinimalUser
    ) where

import Network.HTTP.Conduit (Manager)
import qualified Data.ByteString as BS
import Web.Authenticate.OAuth (OAuth(..), Credential(..))
import Network.HTTP.Types (Status)
import Control.Exception (Exception)
import Data.Typeable (Typeable)

import Web.XING.Types.BirthDate
import Web.XING.Types.User
import Web.XING.Types.User.MinimalUser
import Web.XING.Types.User.FullUser

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
