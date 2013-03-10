{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.XING.API
    ( -- * API request interface
      apiRequest
    , apiBaseUrl
    ) where

import           Network.HTTP.Types (Method)
import           Network.HTTP.Conduit
                   (Response(..), Request(..), parseUrl, httpLbs)
import           Data.Maybe (fromJust)
import           Data.Monoid (mappend)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Resource (MonadResource)
import           Web.XING.Types
import           Web.XING.API.Error
import           Web.XING.Auth
import qualified Control.Exception.Lifted as E
import Control.Exception (throw)
import Network.HTTP.Conduit (HttpException(StatusCodeException))

apiBaseUrl :: BS.ByteString
apiBaseUrl = "https://api.xing.com"

apiRequest
  :: (MonadResource m, MonadBaseControl IO m)
  => OAuth
  -> Manager
  -> AccessToken
  -> Method
  -> BS.ByteString
  -> m (Response BSL.ByteString)
apiRequest oa manager cr m uri  = do
  let req = fromJust $ parseUrl $ BS.unpack (apiBaseUrl `mappend` uri)
  req' <- signOAuth oa cr req{method = m}
  E.catch (httpLbs req' manager)
    (\(StatusCodeException status headers) -> throw $ handleError status headers (m `mappend` " " `mappend` uri))
