{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

import Yesod
import Yesod.Auth
import Yesod.Auth.XING
import qualified Yesod.Auth.Message as Msg
import Network.HTTP.Conduit (newManager, def)
import Data.Text (Text)
import Text.Hamlet (shamlet)
import Data.Monoid (mappend)
import qualified Config
import qualified YesodHelper as Helper

data XINGAuth = XINGAuth {
  httpManager :: Manager
}

instance Yesod XINGAuth where
  approot = ApprootStatic "http://localhost:3000"
  defaultLayout = Helper.bootstrapLayout

mkYesod "XINGAuth" [parseRoutes|
  / RootR GET
  /auth AuthR Auth getAuth
|]

alertMessage
  :: (RenderMessage y msg)
  => msg
  -> GHandler sub y ()
alertMessage message = do
  mr <- getMessageRender
  setMessage [shamlet|
    <div .alert .alert-success>
      <button type=button class=close data-dismiss=alert>&times;
      #{mr message}
  |]

instance YesodAuth XINGAuth where
  type AuthId XINGAuth = Text
  getAuthId creds = return $ lookup "oauth_token" (credsExtra creds)
  loginDest _     = RootR
  logoutDest _    = RootR
  onLogin         = alertMessage Msg.NowLoggedIn
  authPlugins _   = [xingAuth (oauthConsumerKey Config.testConsumer) (oauthConsumerSecret Config.testConsumer)]
  authHttpManager = httpManager

instance RenderMessage XINGAuth FormMessage where
 renderMessage _ _ = defaultFormMessage

getRootR :: Handler RepHtml
getRootR = do
  maid <- maybeAuthId
  defaultLayout $ do
    addScriptRemote "http://code.jquery.com/jquery-1.9.1.min.js"
    addStylesheetRemote $ Helper.bootstrapCDN `mappend` "/css/bootstrap-combined.min.css"
    addScriptRemote     $ Helper.bootstrapCDN `mappend` "/js/bootstrap.min.js"
    setTitle "XING API demo"
    toWidget [julius|
      $(window).ready(function () {
        $('.alert').alert();
      });
    |]
    [whamlet|
      <h1>Welcome to the XING API demo
      $maybe userId <- maid
        <p>Nice to meet you, #{show userId}
        <a href=@{AuthR LogoutR}>Logout
      $nothing
        <p>Hello unknown user. Please log-in.
        <a href=@{AuthR xingLoginRoute}>Login with XING
    |]

main :: IO ()
main = do
  manager <- newManager def
  warpDebug 3000 $ XINGAuth manager
