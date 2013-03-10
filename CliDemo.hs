{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module CliDemo where

import Prelude hiding (putStrLn, putStr, getLine)
import System.IO (hFlush, stdout)
import Data.ByteString (getLine)
import Data.ByteString.Char8 (putStrLn, putStr)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Monoid (mappend)
import Data.Maybe (fromJust, isJust)
import Network.HTTP.Conduit (withManager)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (MonadResource)

import qualified Config
import Web.XING

readVerifier
  :: URL
  -> IO Verifier
readVerifier url = do
  putStrLn "Please confirm the authorization at"
  putStrLn $ "  " `mappend` url
  putStr   "Please enter the PIN: " >> hFlush stdout
  getLine

showAccessToken
  :: (AccessToken, ByteString)
  -> IO ()
showAccessToken (accessToken, userId) = do
  putStrLn $ ""
  putStrLn $ "Hello " `mappend` userId `mappend` "!"
  putStrLn $ "You should put your access token in Config.hs:"
  putStrLn $ ""
  putStrLn $ "accessToken = Just $ newCredential"
  putStrLn $ "  \"" `mappend` (token       accessToken) `mappend` "\""
  putStrLn $ "  \"" `mappend` (tokenSecret accessToken) `mappend` "\""
  putStrLn $ ""

handshake
  :: (MonadResource m, MonadBaseControl IO m)
  => OAuth
  -> Manager
  -> m (RequestToken, URL)
handshake oa manager = do
  (requestToken, url) <- getRequestToken oa manager
  verifier <- liftIO $ readVerifier url
  getAccessToken requestToken verifier oa manager

main :: IO ()
main = do
  putStrLn "XING API demo"
  user <- withManager $ \manager -> do
    accessToken <- if isJust Config.accessToken
      then return $ fromJust Config.accessToken
      else auth manager
    getIdCard Config.testConsumer manager accessToken
  BSL.putStrLn (displayName user)

auth
  :: (MonadResource m, MonadBaseControl IO m)
  => Manager
  -> m RequestToken
auth manager = do
  (accessToken, userId) <- handshake Config.testConsumer manager
  liftIO $ showAccessToken (accessToken, userId)
  return accessToken
