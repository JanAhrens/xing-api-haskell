{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.XING.API
    ( -- * API request interface
      apiRequest
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
import qualified Control.Exception.Lifted as E
import           Web.Authenticate.OAuth (signOAuth)

apiBaseUrl :: BS.ByteString
apiBaseUrl = "https://api.xing.com"

-- | Low level API request interface
apiRequest
  :: (MonadResource m, MonadBaseControl IO m)
  => OAuth         -- ^ OAuth consumer
  -> Manager
  -> AccessToken
  -> Method        -- ^ HTTP verb
  -> BS.ByteString -- ^ HTTP path
  -> m (Response BSL.ByteString)
apiRequest oa manager cr m uri  = do
  let req = fromJust $ parseUrl $ BS.unpack (apiBaseUrl `mappend` uri)
  req' <- signOAuth oa cr req{method = m}
  E.catch (httpLbs req' manager)
    (handleStatusCodeException (m `mappend` " " `mappend` uri))
