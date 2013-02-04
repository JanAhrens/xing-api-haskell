{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.XING.Auth
    ( -- * OAuth related functions
      consumer
    , getRequestToken
    , getRequestToken'
    , Patch.authorizeUrl
    , getAccessToken
    , getAccessToken'
    , Patch.token
    , Patch.tokenSecret
      -- * re-export of some Web.Authenticate.OAuth functions
    , signOAuth
    , newCredential
      -- * re-export of some Web.Authenticate.OAuth types
    , Credential
    , OAuth
    , Manager -- TODO should be exported by Web.XING, b/c not auth specific
    ) where

import qualified Data.ByteString as BS

import Web.Authenticate.OAuth hiding (getAccessToken, getAccessToken')
import qualified Web.XING.Internal.AuthenticateOAuthPatch as Patch

import Control.Monad.Trans.Resource (MonadResource, MonadBaseControl)
import Network.HTTP.Conduit (Manager)
import Control.Monad.Reader (ReaderT, ask)
import Data.Maybe (fromMaybe)

consumer :: BS.ByteString
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

getRequestToken' :: (MonadResource m, MonadBaseControl IO m)
                 => OAuth
                 -> Manager
                 -> m (Credential, BS.ByteString)
getRequestToken' oa manager = do
  requestToken <- Patch.getTemporaryCredential' id oa manager
  return (requestToken, Patch.authorizeUrl oa requestToken)

getRequestToken :: (MonadResource m, MonadBaseControl IO m)
                => ReaderT (OAuth, Manager) m (Credential, BS.ByteString)
getRequestToken = do
  (oa, manager) <- ask
  res <- getRequestToken' oa manager
  return res

getAccessToken' :: (MonadResource m, MonadBaseControl IO m)
                => Credential
                -> BS.ByteString
                -> OAuth
                -> Manager
                -> m (Credential, BS.ByteString)
getAccessToken' requestToken verifier oa manager = do
  accessToken <- Patch.getAccessToken' id oa ((injectVerifier verifier) requestToken) manager
  return (accessToken, fromMaybe "" . lookup "user_id" . unCredential $ accessToken )

getAccessToken :: (MonadResource m, MonadBaseControl IO m)
               => Credential
               -> BS.ByteString
               -> ReaderT (OAuth, Manager) m (Credential, BS.ByteString)
getAccessToken requestToken verifier = do
  (oa, manager) <- ask
  res <- getAccessToken' requestToken verifier oa manager
  return res
