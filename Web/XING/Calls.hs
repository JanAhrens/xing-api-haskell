{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Web.XING.Calls
    ( -- * API request interface
      apiRequest
    , apiBaseUrl
    ) where

import Web.XING.Auth
import Network.HTTP.Types (Method)
import Network.HTTP.Conduit (Response(..), Request(..), parseUrl, httpLbs)
import Control.Monad.Trans.Resource (MonadResource, MonadBaseControl)
import Data.Maybe (fromJust)
import Data.Monoid (mappend)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

apiBaseUrl :: BS.ByteString
apiBaseUrl = "https://api.xing.com"

apiRequest :: (MonadResource m, MonadBaseControl IO m)
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
