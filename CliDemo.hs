{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CliDemo where

import Prelude hiding (putStrLn, putStr, getLine)
import System.IO (hFlush, stdout)
import Data.ByteString (ByteString, getLine)
import Data.ByteString.Char8 (putStrLn, putStr, pack)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader(runReaderT, ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Network.HTTP.Conduit (withManager, Response(..), HttpException(..))
import Network.HTTP.Types (Status(..))
import Data.Monoid (mappend)
import Data.Maybe (fromJust, isJust)
import qualified Control.Exception as E

import qualified Config
import Web.XING
import Web.XING.Auth

readVerifier :: ByteString -> IO ByteString
readVerifier uri = do
  putStrLn "Please confirm the authorization at"
  putStrLn $ "  " `mappend` uri
  putStr   "Please enter the PIN: " >> hFlush stdout
  getLine

showAccessToken :: (Credential, ByteString) -> IO ()
showAccessToken (accessToken, userId) = do
  putStrLn $ ""
  putStrLn $ "Hello " `mappend` userId `mappend` "!"
  putStrLn $ "You should put your access token in Config.hs:"
  putStrLn $ ""
  putStrLn $ "accessToken = Just $ newCredential"
  putStrLn $ "  \"" `mappend` (token       accessToken) `mappend` "\""
  putStrLn $ "  \"" `mappend` (tokenSecret accessToken) `mappend` "\""
  putStrLn $ ""

handshake :: ReaderT (OAuth, Manager) (ResourceT IO) (Credential, ByteString)
handshake = do
  (requestToken, authorizeUri) <- getRequestToken
  verifier <- liftIO $ readVerifier authorizeUri
  getAccessToken requestToken verifier

main :: IO ()
main = do
  putStrLn "XING API demo"
  withManager httpMain
    `E.catch` \(e::HttpException) -> liftIO $ case e of
      StatusCodeException (Status code _) _ -> putStrLn $
        "Failed with code: " `mappend` (pack.show) code
      _ -> putStrLn $
        "Boom. Something unexcepted HTTP related happened: " `mappend` (pack.show) e
  where
    httpMain manager = do
      accessToken <- if (isJust Config.accessToken)
        then return $ fromJust Config.accessToken
        else (auth manager)
      Response _ _ _ body <- apiRequest Config.testConsumer manager accessToken "GET" "/v1/users/me/id_card"
      liftIO $ BSL.putStrLn body

    auth manager = do
      successfulHandshake <- runReaderT handshake (Config.testConsumer, manager)
      liftIO $ showAccessToken successfulHandshake
      return $ fst successfulHandshake
