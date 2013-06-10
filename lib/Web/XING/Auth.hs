{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.XING.Auth
    ( -- * OAuth
      consumer
    , getRequestToken
    , Patch.authorizeUrl
    , getAccessToken
    , Patch.token
    , Patch.tokenSecret
      -- reexports of some Web.Authenticate.OAuth functions
    , newCredential -- | (re)create an OAuth token (for example access token)
    , oauthCallback -- | extract the OAuth callback from a consumer
    ) where

import Prelude
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString as BS

import Web.Authenticate.OAuth hiding (getAccessToken)
import qualified Web.XING.Internal.AuthenticateOAuthPatch as Patch
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (MonadResource)

import Data.Maybe (fromMaybe)

import Web.XING.Types
import Web.XING.API.Error

-- | Create an OAuth consumer
consumer
  :: BS.ByteString -- ^ consumer key
  -> BS.ByteString -- ^ consumer secret
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

-- | Create a request token (/temporary credentials/)
getRequestToken
 :: (MonadResource m, MonadBaseControl IO m)
 => OAuth -- ^ OAuth consumer
 -> Manager
 -> m (RequestToken, URL) -- ^ request token and authorize URL
getRequestToken oa manager = E.catch
    tryToGetRequestToken
    (handleStatusCodeException "POST /v1/request_token")
  where
    tryToGetRequestToken = do
      cred <- Patch.getTemporaryCredential' id oa manager
      return (cred, Patch.authorizeUrl oa cred)

-- | Exchange request token for an access token (/token credentials/)
getAccessToken
  :: (MonadResource m, MonadBaseControl IO m)
  => RequestToken -- ^ request token obtained from 'getRequestToken'
  -> Verifier     -- ^ verifier obtained by calling 'Patch.authorizeUrl'
  -> OAuth        -- ^ OAuth consumer (see 'consumer')
  -> Manager
  -> m (Credential, BS.ByteString)
getAccessToken requestToken verifier oa manager = E.catch
    tryToExchangeRequestToken
    (handleStatusCodeException "POST /v1/access_token")
  where
    tryToExchangeRequestToken = do
      accessToken <- Patch.getAccessToken' id oa ((injectVerifier verifier) requestToken) manager
      return (accessToken, fromMaybe "" . lookup "user_id" . unCredential $ accessToken)
