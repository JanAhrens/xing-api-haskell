{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

import Yesod
import qualified Config
import Web.XING
import Network.HTTP.Conduit (newManager, def)
import Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Monoid (mappend)
import Data.Text.Encoding as E
import Data.Maybe (isJust, fromMaybe)

data HelloXING = HelloXING {
    httpManager :: Manager
  , oAuthConsumer :: OAuth
}

instance Yesod HelloXING

getRequestToken' yesod = do
  let oa = oAuthConsumer yesod
  let manager = httpManager yesod
  getRequestToken oa manager

mkYesod "HelloXING" [parseRoutes|
/ HomeR GET
/handshake HandshakeR POST
/callback CallbackR GET
/logout LogoutR POST
|]

postLogoutR :: Handler RepHtml
postLogoutR = do
  deleteSession "accessToken"
  deleteSession "accessTokenSecret"
  redirect HomeR

postHandshakeR :: Handler RepHtml
postHandshakeR = do
  yesod <- getYesod
  (requestToken, url) <- getRequestToken' yesod
  setSessionBS "requestToken"  (token requestToken)
  setSessionBS "requestSecret" (tokenSecret requestToken)

  redirect $ E.decodeUtf8 url

getCallbackR :: Handler RepHtml
getCallbackR = do
  yesod <- getYesod
  let oa = oAuthConsumer yesod
  let manager = httpManager yesod
  maybeRequestToken <- lookupSessionBS "requestToken"
  maybeRequestTokenSecret <- lookupSessionBS "requestSecret"
  maybeVerifier <- lookupGetParam "oauth_verifier"
  let verifier = E.encodeUtf8 (fromMaybe "" maybeVerifier)
  if (isJust maybeRequestToken && isJust maybeRequestTokenSecret)
    then do
      let requestToken = (newCredential (fromJust maybeRequestToken) (fromJust maybeRequestTokenSecret))
      (accessToken, _) <- getAccessToken requestToken verifier oa manager
      setSessionBS "accessToken" (token accessToken)
      setSessionBS "accessTokenSecret" (tokenSecret accessToken)
    else return ()
  redirect HomeR

getHomeR :: Handler RepHtml
getHomeR = do
  yesod <- getYesod
  let oa = oAuthConsumer yesod
  let manager = httpManager yesod
  maybeToken  <- lookupSessionBS "accessToken"
  maybeSecret <- lookupSessionBS "accessTokenSecret"
  if (isJust maybeToken && isJust maybeSecret)
    then do
      let accessToken = (newCredential (fromJust maybeToken) (fromJust maybeSecret))
      idCard <- getIdCard oa manager accessToken
      defaultLayout [whamlet|
        <h1>Callback
        <p>Nice to meet you, #{BSL.unpack $ displayName idCard}.
        <form method=POST action=@{LogoutR}>
          <input type=submit value="Logout">
      |]
    else do
      defaultLayout [whamlet|
        <h1>Hello
        <form method=POST action=@{HandshakeR}>
          <input type=submit value="Login with XING">
      |]

main :: IO ()
main = do
  manager <- newManager def
  let port = 3000
  let testConsumer = Config.testConsumer{
    oauthCallback = Just $ "http://localhost:" `mappend` (BS.pack.show) port `mappend` "/callback"
  }
  warpDebug port (HelloXING manager testConsumer)
