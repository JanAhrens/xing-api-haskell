{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.XING.API
    ( -- * API request interface
      apiRequest
    , withAPI
    , apiBaseUrl
    ) where

import Web.XING.Auth
import Network.HTTP.Types (Method)
import Network.HTTP.Conduit
    ( Response(..), Request(..), parseUrl, httpLbs
    , withManager, HttpException(..)
    )
import Control.Monad.Trans.Resource (MonadResource, MonadBaseControl)
import Data.Maybe (fromJust)
import Data.Monoid (mappend)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ResourceT)
import Network.HTTP.Types (Status(..))
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

apiBaseUrl :: BS.ByteString
apiBaseUrl = "https://api.xing.com"

apiRequest
  :: (MonadResource m, MonadBaseControl IO m)
  => OAuth
  -> Manager
  -> Credential
  -> Method
  -> BS.ByteString
  -> m (Response BSL.ByteString)
apiRequest oa manager cr m uri  = do
  let req = fromJust $ parseUrl $ BS.unpack (apiBaseUrl `mappend` uri)
  req' <- signOAuth oa cr req{method = m}
  rsp <- httpLbs req' manager
  return rsp

withAPI
  :: (Manager -> ResourceT IO ())
  -> IO ()
withAPI main = do
  E.catch (withManager main) (\(e::HttpException) -> liftIO $ handleError e)
  where
    handleError (StatusCodeException status _) = putStrLn $ "Failed with code: " ++ show (statusCode status)
    handleError e                              = putStrLn $ "Something unexcepted happened: " ++ show e
