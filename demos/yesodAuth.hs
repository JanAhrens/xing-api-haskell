{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

import Yesod
import Yesod.Auth.XING
import Yesod.Auth (YesodAuth(..), maybeAuthId, getAuth, Auth, Route(PluginR, LogoutR),
                    AuthId, credsIdent, Creds(..))
import Network.HTTP.Conduit (newManager, def)
import Data.Text (Text)

import qualified Config

data XINGAuth = XINGAuth {
  httpManager :: Manager
}

instance Yesod XINGAuth where
  approot = ApprootStatic "http://localhost:3000"

mkYesod "XINGAuth" [parseRoutes|
  / RootR GET
  /auth AuthR Auth getAuth
|]

instance YesodAuth XINGAuth where
  type AuthId XINGAuth = Text
  getAuthId       = return . Just . credsIdent
  loginDest _     = RootR
  logoutDest _    = RootR
  authPlugins _   = [xingAuth (oauthConsumerKey Config.testConsumer) (oauthConsumerSecret Config.testConsumer)]
  authHttpManager = httpManager

instance RenderMessage XINGAuth FormMessage where
 renderMessage _ _ = defaultFormMessage

getRootR :: Handler RepHtml
getRootR = do
  maid <- maybeAuthId
  widget <- case maid of
    Just a -> return $ whoAmI a
    Nothing -> return pleaseLogIn
  defaultLayout [whamlet|
    <h1>Hello
    ^{widget}
  |]


pleaseLogIn :: Widget
pleaseLogIn = do
  let xingAuthRoute = AuthR $ PluginR "xing" ["forward"]
  toWidget [hamlet|
    <p>Hello unknown user. Please log-in.
    <a href=@{xingAuthRoute}>Login with XING
  |]

whoAmI
  :: (Show a)
  => a
  -> Widget
whoAmI userId = do
  toWidget [hamlet|
    <p>Your current auth ID: #{show userId}
    <a href=@{AuthR LogoutR}>Logout
  |]

main :: IO ()
main = do
  manager <- newManager def
  warpDebug 3000 $ XINGAuth manager
