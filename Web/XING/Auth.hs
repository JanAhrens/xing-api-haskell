{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.XING.Auth
    ( -- * OAuth related functions
      consumer
    , getRequestToken
    , Patch.authorizeUrl
    , getAccessToken
    , Patch.token
    , Patch.tokenSecret
      -- * re-export of some Web.Authenticate.OAuth functions
    , signOAuth
    , newCredential
    , oauthCallback
    ) where

import Prelude
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString as BS

import Web.Authenticate.OAuth hiding (getAccessToken)
import qualified Web.XING.Internal.AuthenticateOAuthPatch as Patch
import Control.Exception (throw)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (MonadResource)
import Network.HTTP.Conduit (HttpException(StatusCodeException))

import Data.Maybe (fromMaybe)

import Web.XING.Types
import Web.XING.API.Error

consumer
  :: BS.ByteString
  -> BS.ByteString
  -> OAuth
consumer consumerKey consumerSecret = newOAuth
  { oauthRequestUri      = "https://api.xing.com/v1/request_token"
  , oauthAccessTokenUri  = "https://api.xing.com/v1/access_token"
  , oauthAuthorizeUri    = "https://api.xing.com/v1/authorize"
  , oauthSignatureMethod = PLAINTEXT
  , oauthVersion         = OAuth10a
  , oauthConsumerKey     = consumerKey
  , oauthConsumerSecret  = consumerSecret
  , oauthCallback        = Just "oob"
  }

getRequestToken
 :: (MonadResource m, MonadBaseControl IO m)
 => OAuth
 -> Manager
 -> m (RequestToken, URL)
getRequestToken oa manager = E.catch
    tryToGetRequestToken
    (\(StatusCodeException status headers) -> throw $ handleError status headers "POST /v1/request_token")
  where
    tryToGetRequestToken = do
      cred <- Patch.getTemporaryCredential' id oa manager
      return (cred, Patch.authorizeUrl oa cred)

getAccessToken
  :: (MonadResource m, MonadBaseControl IO m)
  => RequestToken
  -> Verifier
  -> OAuth
  -> Manager
  -> m (Credential, BS.ByteString)
getAccessToken requestToken verifier oa manager = E.catch
    tryToExchangeRequestToken
    (\(StatusCodeException status headers) -> throw $ handleError status headers "POST /v1/access_token")
  where
    tryToExchangeRequestToken = do
      accessToken <- Patch.getAccessToken' id oa ((injectVerifier verifier) requestToken) manager
      return (accessToken, fromMaybe "" . lookup "user_id" . unCredential $ accessToken)
