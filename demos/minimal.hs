{-# LANGUAGE OverloadedStrings #-}

import Web.XING
import Control.Monad.IO.Class (MonadIO (liftIO))
import Network.HTTP.Conduit (withManager)
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
