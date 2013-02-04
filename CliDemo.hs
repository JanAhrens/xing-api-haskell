{-# LANGUAGE OverloadedStrings #-}

module CliDemo where

import Prelude hiding (putStrLn, putStr, getLine)
import System.IO (hFlush, stdout)
import Data.ByteString (ByteString, getLine)
import Data.ByteString.Char8 (putStrLn, putStr)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader(runReaderT, ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Monoid (mappend)
import Data.Maybe (fromJust, isJust)

import qualified Config
import Web.XING

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
  withAPI $ \manager -> do
    accessToken <- if (isJust Config.accessToken)
      then return $ fromJust Config.accessToken
      else (auth manager)
    idCard <- getIdCard Config.testConsumer manager accessToken
    liftIO $ case idCard of
      Just a  -> BSL.putStrLn $ "Hello " `mappend` (displayName a)
      Nothing -> BSL.putStrLn "failed to fetch id_card"
  where
    auth manager = do
      successfulHandshake <- runReaderT handshake (Config.testConsumer, manager)
      liftIO $ showAccessToken successfulHandshake
      return $ fst successfulHandshake
