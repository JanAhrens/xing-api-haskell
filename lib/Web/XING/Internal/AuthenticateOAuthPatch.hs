{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Web.XING.Internal.AuthenticateOAuthPatch
    ( -- * changed Web.Authenticate.OAuth functions
      getTemporaryCredential'
    , authorizeUrl
    , getAccessToken'
      -- * accessor functions for Credential (from Web.Authenticate.OAuth)
    , token
    , tokenSecret
    ) where

import Web.Authenticate.OAuth hiding (authorizeUrl, getAccessToken', getTemporaryCredential')
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Control.Monad.Trans.Resource (MonadResource, MonadBaseControl)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (throwIO)
import Network.HTTP.Conduit (Manager, Request(method), httpLbs, responseStatus, responseBody, parseUrl)
import Network.HTTP.Types (status201, parseSimpleQuery)
import Data.Maybe (fromJust, fromMaybe)

-- | extract the token from the 'Credential'
token :: Credential -> BS.ByteString
token = fromMaybe "" . lookup "oauth_token" . unCredential

-- | extract the secret from the 'Credential'
tokenSecret :: Credential -> BS.ByteString
tokenSecret = fromMaybe "" . lookup "oauth_token_secret" . unCredential

toStrict :: BSL.ByteString -> BS.ByteString
toStrict = BS.concat . BSL.toChunks

-- we can't use OA.getTemporaryCredential', because the XING API returns 201 instead of 200
getTemporaryCredential' :: (MonadResource m, MonadBaseControl IO m)
                => (Request -> Request)
                -> OAuth
                -> Manager
                -> m Credential
getTemporaryCredential' hook oa manager = do
  let req = fromJust $ parseUrl $ oauthRequestUri oa
      crd = maybe id (insert "oauth_callback") (oauthCallback oa) $ emptyCredential
  req' <- signOAuth oa crd $ hook (req { method = "POST" })
  rsp <- httpLbs req' manager
  if responseStatus rsp == status201
    then do
      let dic = parseSimpleQuery . toStrict . responseBody $ rsp
      return $ Credential dic
    else liftIO . throwIO . OAuthException $ "Gaining OAuth Temporary Credential Failed: " ++ BSL.unpack (responseBody rsp)

-- | URL to obtain OAuth verifier
authorizeUrl :: OAuth         -- ^ OAuth consumer
             -> Credential    -- ^ request token 'Web.XING.getRequestToken'
             -> BS.ByteString -- ^ URL to send the user to
authorizeUrl consumer = BS.pack . (authorizeUrl' (\_ -> const []) consumer{oauthCallback=Nothing})

-- we can't use OA.getAccessToken, because the XING API returns 201 instead of 200
getAccessToken' :: (MonadResource m, MonadBaseControl IO m)
                => (Request -> Request)
                -> OAuth
                -> Credential
                -> Manager
                -> m Credential
getAccessToken' hook oa cr manager = do
  let req = hook (fromJust $ parseUrl $ oauthAccessTokenUri oa) { method = "POST" }
  rsp <- flip httpLbs manager =<< signOAuth oa (if oauthVersion oa == OAuth10 then delete "oauth_verifier" cr else cr) req
  if responseStatus rsp == status201
    then do
      let dic = parseSimpleQuery . toStrict . responseBody $ rsp
      return $ Credential dic
    else liftIO . throwIO . OAuthException $ "Gaining OAuth Token Credential Failed: " ++ BSL.unpack (responseBody rsp)
