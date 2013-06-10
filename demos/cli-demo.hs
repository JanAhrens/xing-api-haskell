-- This demo shows how to authenticate users using OAuth and how to
-- reuse the access tokens.
-- It used the OAuth out of band (OOB) mode and can be used as a starting
-- point to write an application that does not to have a web view.
--
-- Make sure to set the environment variables XING_CONSUMER_KEY and
-- XING_CONSUMER_SECRET before trying this demo.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.IO (hFlush, stdout)
import Data.Maybe (fromJust, isJust)

import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (MonadResource)

import Data.Monoid (mappend)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.IO as T

import Web.XING

import System.Environment (getEnv)
import qualified System.IO.Error
import qualified Control.Exception as E

readVerifier
  :: URL
  -> IO Verifier
readVerifier url = do
  putStrLn "Please confirm the authorization at"
  putStrLn $ "  " `mappend` (BS.unpack url)
  putStr   "Please enter the PIN: " >> hFlush stdout
  BS.getLine

showAccessToken
  :: (AccessToken, BS.ByteString)
  -> IO ()
showAccessToken (accessToken, uid) = do
  putStrLn $ ""
  putStrLn $ "Hello " `mappend` (BS.unpack uid) `mappend` "!"
  putStrLn $ "You should set the access token as environment variables:"
  putStrLn $ ""
  putStrLn $ "  export XING_ACCESS_TOKEN_KEY=\""    `mappend` ((BS.unpack.token)       accessToken) `mappend` "\""
  putStrLn $ "  export XING_ACCESS_TOKEN_SECRET=\"" `mappend` ((BS.unpack.tokenSecret) accessToken) `mappend` "\""
  putStrLn $ ""

handshake
  :: (MonadResource m, MonadBaseControl IO m)
  => OAuth
  -> Manager
  -> m (AccessToken, BS.ByteString)
handshake oa manager = do
  (requestToken, url) <- getRequestToken oa manager
  verifier <- liftIO $ readVerifier url
  getAccessToken requestToken verifier oa manager

readAccessToken :: IO (Maybe Credential)
readAccessToken = do
  access_token_key    <- getEnv "XING_ACCESS_TOKEN_KEY"
  access_token_secret <- getEnv "XING_ACCESS_TOKEN_SECRET"
  return $ Just $ newCredential (BS.pack access_token_key) (BS.pack access_token_secret)

main :: IO ()
main = do
  consumer_key    <- getEnv "XING_CONSUMER_KEY"
  consumer_secret <- getEnv "XING_CONSUMER_SECRET"
  let xingConsumer = consumer (BS.pack consumer_key) (BS.pack consumer_secret)

  maybeAccessToken <- E.catch (readAccessToken) (\(_ :: System.IO.Error.IOError) -> return Nothing)

  putStrLn "XING API demo"
  user <- withManager $ \manager -> do
    accessToken <- if isJust maybeAccessToken
      then return $ fromJust maybeAccessToken
      else auth xingConsumer manager
    getIdCard xingConsumer manager accessToken
  T.putStrLn (displayName user)

auth
  :: (MonadResource m, MonadBaseControl IO m)
  => OAuth
  -> Manager
  -> m RequestToken
auth oa manager = do
  res@(accessToken, _) <- handshake oa manager
  liftIO $ showAccessToken res
  return accessToken
