{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CliDemo where

import Prelude hiding (putStrLn, putStr, getLine)
import System.IO (hFlush, stdout)
import Data.ByteString (ByteString, getLine)
import Data.ByteString.Char8 (putStrLn, putStr)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader(runReaderT, ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Network.HTTP.Conduit (withManager, Manager)
import Data.Monoid (mappend)
import Data.Maybe (fromJust)
import qualified Control.Exception as E

import Config
import Web.XING.Auth

readVerifier :: ByteString -> IO ByteString
readVerifier uri = do
  putStrLn "Please confirm the authorization at"
  putStrLn $ "  " `mappend` uri
  putStr   "Please enter the PIN: " >> hFlush stdout
  getLine

showAccessToken :: (Credential, ByteString) -> IO ()
showAccessToken (accessToken, userId) = do
  putStrLn $ "Hello " `mappend` userId `mappend` "!"
  putStrLn "Here is your access token:"
  putStrLn $ "Token:  " `mappend` (token accessToken)
  putStrLn $ "Secret: " `mappend` (tokenSecret accessToken)

handshake :: ReaderT (OAuth, Manager) (ResourceT IO) (Credential, ByteString)
handshake = do
  (requestToken, authorizeUri) <- getRequestToken
  verifier <- liftIO $ readVerifier authorizeUri
  getAccessToken requestToken verifier

main :: IO ()
main = do
  putStrLn "XING API demo"
  withManager auth
    `E.catch` \(_::E.SomeException) -> liftIO $ putStrLn "Failed"
  where
    auth manager = do
      successfulHandshake <- runReaderT handshake (testConsumer, manager)
      liftIO $ showAccessToken successfulHandshake
