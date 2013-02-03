{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

import Yesod
import Config
import Web.XING.Calls.IdCard
import Network.HTTP.Conduit (Manager, newManager, def)
import Data.Maybe (fromJust)
import Data.ByteString.Lazy.Char8 (unpack)

data HelloXING = HelloXING {
    httpManager :: Manager
}

instance Yesod HelloXING

mkYesod "HelloXING" [parseRoutes|
/ HomeR GET
|]

getHomeR :: Handler RepHtml
getHomeR = do
  yesod <- getYesod
  maybeIdCard <- getIdCard Config.testConsumer (httpManager yesod) (fromJust Config.accessToken)
  defaultLayout [whamlet|
    <h1>Hello XING
    $maybe idCard <- maybeIdCard
      <p>Nice to meet you, #{unpack $ displayName idCard}.
    $nothing
      <p>Sorry but something went wrong.
  |]

main :: IO ()
main = do
  manager <- newManager def
  warpDebug 3000 (HelloXING manager)
