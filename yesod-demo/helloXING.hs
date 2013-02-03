{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

import Yesod
import Config
import Web.XING
import Network.HTTP.Conduit (Manager, newManager, def, Response(..))
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
  Response _ _ _ body <- apiRequest Config.testConsumer (httpManager yesod)
                                    (fromJust Config.accessToken)
                                    "GET" "/v1/users/me/id_card"
  defaultLayout [whamlet|
    <h1>Hello XING
    <code>#{unpack body}
  |]

main :: IO ()
main = do
  manager <- newManager def
  warpDebug 3000 (HelloXING manager)
