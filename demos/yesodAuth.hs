{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

import Yesod
import Yesod.Auth.XING
import Yesod.Auth (YesodAuth(..), maybeAuthId, getAuth, Auth, Route(LoginR, LogoutR),
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
  defaultLayout [whamlet|
    <p>Your current auth ID: #{show maid}
    $maybe _ <- maid
      <p>
        <a href=@{AuthR LogoutR}>Logout
    $nothing
      <p>
        <a href=@{AuthR LoginR}>Go to the login page
  |]

main :: IO ()
main = do
  man <- newManager def
  warpDebug 3000 $ XINGAuth man
