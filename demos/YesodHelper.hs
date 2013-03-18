{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module YesodHelper(
    bootstrapCDN
  , bootstrapLayout
  , alertMessage
  , writeTokenToSession
  , getTokenFromSession
  , deleteTokenFromSession
) where

import Yesod
import Data.Text (Text)
import Web.XING
import Text.Hamlet (shamlet)
import Data.Maybe (isJust, fromJust)
import Data.Monoid (mappend)

bootstrapCDN :: Text
bootstrapCDN = "//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.0"

bootstrapLayout
  :: (Yesod a)
  => GWidget sub a ()
  -> GHandler sub a RepHtml
bootstrapLayout widget = do
    pc <- widgetToPageContent widget
    mmsg <- getMessage
    hamletToRepHtml [hamlet|
      $doctype 5
      <html>
        <head>
          <title>#{pageTitle pc}
          ^{pageHead pc}
        <body>
          <div .hero-unit>
            <h1>#{pageTitle pc}
            $maybe msg <- mmsg
              #{msg}
            ^{pageBody pc}
    |]

alertMessage
  :: (RenderMessage y msg)
  => msg
  -> GHandler sub y ()
alertMessage message = do
  mr <- getMessageRender
  setMessage [shamlet|
    <p .alert .alert-success>
      <button type=button class=close data-dismiss=alert>&times;
      #{mr message}
  |]

writeTokenToSession
  :: Text
  -> Credential
  -> GHandler sub a ()
writeTokenToSession name credential = do
  setSessionBS (name `mappend` "Token") (token credential)
  setSessionBS (name `mappend` "Secret") (tokenSecret credential)

getTokenFromSession
  :: Text
  -> GHandler sub a (Maybe Credential)
getTokenFromSession name = do
  maybeToken  <- lookupSessionBS (name `mappend` "Token")
  maybeSecret <- lookupSessionBS (name `mappend` "Secret")
  if (isJust maybeToken && isJust maybeSecret)
    then return $ Just (newCredential (fromJust maybeToken) (fromJust maybeSecret))
    else return Nothing

deleteTokenFromSession
  :: Text
  -> GHandler sub a ()
deleteTokenFromSession name = do
  deleteSession (name `mappend` "Token")
  deleteSession (name `mappend` "Secret")
