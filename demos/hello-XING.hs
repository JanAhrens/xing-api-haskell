-- This demo shows how to use Yesod with the XING API.
--
-- The OAuth handshake is implemented without using yesod-auth.
-- Have a look at the yesod-auth-xing package for a simpler
-- solution to authenticate users using the XING API.
--
-- Make sure to setup your Config.hs file before trying this demo.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

-- TODO: Web.XING.Types.User.FullUser also exports languages. Find a better name.
import           Yesod.Core hiding (languages)
import           Network.Wai.Handler.Warp (run)
import           Text.Hamlet (hamlet)
import           Web.XING
import           Network.HTTP.Conduit (newManager, def)
import           Data.Maybe (fromJust, isJust, fromMaybe)
import qualified Data.Map as M
import           Helper.YesodHelper ( bootstrapLayout, bootstrapCDN
                                    , writeTokenToSession, getTokenFromSession
                                    , deleteTokenFromSession )
import qualified Data.ByteString.Char8 as BS
import           Data.Monoid (mappend)
import qualified Data.Text.Encoding as E
import           Data.Time
import qualified Config
import qualified Data.Text as T

data HelloXING = HelloXING {
    httpManager   :: Manager
  , oAuthConsumer :: OAuth
}

instance Yesod HelloXING where
  defaultLayout = bootstrapLayout

mkYesod "HelloXING" [parseRoutes|
  /          HomeR      GET
  /handshake HandshakeR POST
  /callback  CallbackR  GET
  /logout    LogoutR    POST
|]

postHandshakeR :: Handler RepHtml
postHandshakeR = do
  yesod <- getYesod
  let oa = oAuthConsumer yesod
  let manager = httpManager yesod

  (requestToken, url) <- getRequestToken oa manager
  writeTokenToSession "request" requestToken

  redirect $ E.decodeUtf8 url

getCallbackR :: Handler RepHtml
getCallbackR = do
  yesod <- getYesod
  let oa = oAuthConsumer yesod
  let manager = httpManager yesod

  maybeRequestToken <- getTokenFromSession "request"

  maybeVerifier <- lookupGetParam "oauth_verifier"
  let verifier = E.encodeUtf8 (fromMaybe "" maybeVerifier)

  if isJust maybeRequestToken
    then do
      (accessToken, _) <- getAccessToken (fromJust maybeRequestToken) verifier oa manager
      writeTokenToSession "access" accessToken
    else return ()

  redirect HomeR

postLogoutR :: Handler RepHtml
postLogoutR = do
  deleteTokenFromSession "access"
  deleteTokenFromSession "request"
  redirect HomeR

daysUntilBirthday
  :: Maybe BirthDate
  -> Day
  -> Integer
daysUntilBirthday (Just (DayOnly birthMonth birthDay)) today    = calcDays birthMonth birthDay today
daysUntilBirthday (Just (FullDate _ birthMonth birthDay)) today = calcDays birthMonth birthDay today
daysUntilBirthday Nothing _                                     = 0

calcDays
  :: Int
  -> Int
  -> Day
  -> Integer
calcDays birthMonth birthDay today
  = if (birthDayThisYear >= 0)
      then birthDayThisYear
      else birthDayNextYear
  where
    (year, _, _)     = toGregorian today
    birthDayThisYear = diffDays (fromGregorian (year    ) birthMonth birthDay) today
    birthDayNextYear = diffDays (fromGregorian (year + 1) birthMonth birthDay) today

getHomeR :: Handler RepHtml
getHomeR = do
  maybeAccessToken <- getTokenFromSession "access"

  widget <- case maybeAccessToken of
    Just accessToken -> do
      yesod <- getYesod
      users <- getUsers (oAuthConsumer yesod) (httpManager yesod) accessToken ["me"]
      today <- liftIO $ getCurrentTime
      let firstUser = head $ unUserList users
      let birthDayInDays = daysUntilBirthday (birthDate firstUser) (utctDay today)
      return $ whoAmI firstUser birthDayInDays

    Nothing -> return pleaseLogIn

  defaultLayout $ do
    addStylesheetRemote $ bootstrapCDN `mappend` "/css/bootstrap-combined.min.css"
    [whamlet|
      <h1>Welcome to the XING API demo
      ^{widget}
    |]

pleaseLogIn :: Widget
pleaseLogIn =
  toWidget [hamlet|
    <img src="https://www.xing.com/img/n/nobody_m.png">
    <p>Hello unknown user. Please log-in.
    <form method=POST action=@{HandshakeR}>
      <input type=submit value="Login with XING">
  |]

whoAmI
  :: FullUser
  -> Integer
  -> Widget
whoAmI user birthDayInDays = do
  toWidget [hamlet|
    <img src=#{fromMaybe "" $ M.lookup "large" (photoUrls user)}>
    <p>
      <a href=#{permalink user}>#{displayName user}
    <p>
      Hey #{firstName user}! Welcome to this demo.<br>


    <p>
      Your next birthday is in #{show birthDayInDays} days.

    <p>
      Did you know? In Germany you would be greeted with "Guten Tag 
      $if (gender user) == Male
        Herr
      $else
        Frau
      \ #{lastName user}".

    $if (length (M.keys (languages user)) > 1)
      <p>
        Impressive! You are speaking #{length (M.keys (languages user))} languages:
        \  #{T.intercalate ", " (M.keys (languages user))}.

    $maybe mail <- activeEmail user
      <p>Your active email address is <a href=mailto:#{mail}>#{mail}</a>.

    <p>Here is a list of your premium services
    <ul>
      $forall service <- premiumServices user
        <li>#{service}

    $if null $ badges user
      You have no badges.
    $else
      <p>Your badges:
      <ul>
        $forall badge <- badges user
          <li>#{badge}

    <form method=POST action=@{LogoutR}>
      <input type=submit value="Logout">
  |]

main :: IO ()
main = do
  manager <- newManager def
  let port = 3000
  let testConsumer = Config.testConsumer{
    oauthCallback = Just $ "http://localhost:" `mappend` (BS.pack.show) port `mappend` "/callback"
  }
  putStrLn $ "Starting on port " ++ show port
  run port =<< toWaiApp (HelloXING manager testConsumer)
