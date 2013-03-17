{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Yesod.Auth.XING (
    xingAuth
  , xingLoginRoute
  , module Web.XING
) where


import Web.XING
import Yesod.Handler (setSessionBS, getYesod, lift, getUrlRender, getRouteToMaster, GHandler, lookupSessionBS, redirect, notFound)
import Yesod.Request (lookupGetParam)
import Yesod.Auth (AuthPlugin(..), YesodAuth(..), Route(PluginR),
                    setCreds, Creds(..), Auth)
import Text.Hamlet (shamlet)
import Yesod.Widget (toWidget)
import Data.Text (Text)
import Data.Maybe (isJust, fromJust, fromMaybe)
import Data.Monoid (mappend)
import Control.Arrow ((***))

import qualified Data.ByteString as BS
import Data.Text.Encoding (decodeUtf8With, decodeUtf8, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)

bsToText :: ByteString -> Text
bsToText = decodeUtf8With lenientDecode

writeTokenToSession
  :: YesodAuth m
  => Text
  -> Credential
  -> GHandler s m ()
writeTokenToSession name credential = do
  setSessionBS (name `mappend` "Token") (token credential)
  setSessionBS (name `mappend` "Secret") (tokenSecret credential)

getTokenFromSession
  :: YesodAuth m
  => Text
  -> GHandler s m (Maybe Credential)
getTokenFromSession name = do
  maybeToken  <- lookupSessionBS (name `mappend` "Token")
  maybeSecret <- lookupSessionBS (name `mappend` "Secret")
  if (isJust maybeToken && isJust maybeSecret)
    then return $ Just (newCredential (fromJust maybeToken) (fromJust maybeSecret))
    else return Nothing

xingLoginRoute :: Route Auth
xingLoginRoute = PluginR "xing" ["forward"]

xingAuth
  :: YesodAuth m
  => BS.ByteString -- ^ key
  -> BS.ByteString -- ^ secret
  -> AuthPlugin m
xingAuth key secret = AuthPlugin name dispatch login
  where
    name = "xing"
    url = PluginR name []
    oauth = consumer key secret

    dispatch "GET" ["forward"] = do
      render <- getUrlRender
      tm <- getRouteToMaster
      let oauth' = oauth { oauthCallback = Just $ encodeUtf8 $ render $ tm url }
      master <- getYesod
      let manager = authHttpManager master
      (requestToken, authorizeRoute) <- getRequestToken oauth' manager
      writeTokenToSession "request" requestToken
      redirect $ decodeUtf8 authorizeRoute

    dispatch "GET" [] = do
      maybeVerifier <- lookupGetParam "oauth_verifier"
      let verifier = encodeUtf8 (fromMaybe "" maybeVerifier)
      maybeRequestToken <- getTokenFromSession "request"
      master <- getYesod
      let manager = authHttpManager master
      (accessToken, userId) <- getAccessToken (fromJust maybeRequestToken) verifier oauth manager
      let creds = Creds name (decodeUtf8 userId) (map (bsToText *** bsToText) (unCredential accessToken))
      setCreds True creds

    dispatch _ _ = notFound

    login tm = do
      render <- lift getUrlRender
      let pluginUrl = render $ tm $ PluginR "xing" ["forward"]
      toWidget [shamlet|
        <a href=#{pluginUrl}>Login with XING
      |]
