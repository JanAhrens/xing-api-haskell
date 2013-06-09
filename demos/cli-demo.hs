-- This demo shows how to authenticate users using OAuth and how to
-- reuse the access tokens.
-- It used the OAuth out of band (OOB) mode and can be used as a starting
-- point to write an application that does not to have a web view.
--
-- Make sure to setup your Config.hs file before trying this demo.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.IO (hFlush, stdout)
import Data.Maybe (fromJust, isJust)

import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (MonadResource)

import Data.Monoid (mappend)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.IO as T

import Web.XING
import qualified Config

readVerifier
  :: URL
  -> IO Verifier
readVerifier url = do
  putStrLn "Please confirm the authorization at"
  putStrLn $ "  " `mappend` (BS.unpack url)
  putStr   "Please enter the PIN: " >> hFlush stdout
  BS.getLine

showAccessToken
  :: (AccessToken, ByteString)
  -> IO ()
showAccessToken (accessToken, uid) = do
  putStrLn $ ""
  putStrLn $ "Hello " `mappend` (BS.unpack uid) `mappend` "!"
  putStrLn $ "You should put your access token in Config.hs:"
  putStrLn $ ""
  putStrLn $ "accessToken = Just $ newCredential"
  putStrLn $ "  \"" `mappend` ((BS.unpack.token)       accessToken) `mappend` "\""
  putStrLn $ "  \"" `mappend` ((BS.unpack.tokenSecret) accessToken) `mappend` "\""
  putStrLn $ ""

handshake
  :: (MonadResource m, MonadBaseControl IO m)
  => OAuth
  -> Manager
  -> m (AccessToken, ByteString)
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
  T.putStrLn (displayName user)

auth
  :: (MonadResource m, MonadBaseControl IO m)
  => Manager
  -> m RequestToken
auth manager = do
  res@(accessToken, _) <- handshake Config.testConsumer manager
  liftIO $ showAccessToken res
  return accessToken