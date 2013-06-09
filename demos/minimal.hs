-- This is a minimal example that can fit in the README and shows how to
-- do the OAuth handshake and call a protected resource.
-- It's not the best example since it's not reusing access tokens.
-- Have a look at the cli-demo.hs file for a more complex example.

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.XING
import qualified Data.ByteString.Char8 as BS

oauthConfig :: OAuth
oauthConfig = consumer "YOUR_CONSUMER_KEY" "YOUR_CONSUMER_SECRET"

main :: IO ()
main = withManager $ \manager -> do
  (accessToken, _) <- handshake manager
  idCard <- getIdCard oauthConfig manager accessToken
  liftIO $ putStrLn $ show idCard
  where
    handshake manager = do
      (requestToken, url) <- getRequestToken oauthConfig manager
      verifier <- liftIO $ do
        BS.putStrLn url
        BS.putStr "PIN: "
        BS.getLine
      getAccessToken requestToken verifier oauthConfig manager
