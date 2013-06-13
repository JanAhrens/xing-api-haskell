{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Web.XING.Calls.IdCard
    (
      getIdCard
    , demoIdCard
    , demoIdCard'
    ) where

import Web.XING.Types
import Web.XING.API

import qualified Data.ByteString.Lazy.Char8 as BSL
import Network.HTTP.Conduit (Response(..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (MonadResource)
import Control.Exception (throw)
import Data.Aeson (encode, decode, Value(..), object, (.=))

-- | Get your id card <https://dev.xing.com/docs/get/users/me/id_card>
getIdCard
  :: (MonadResource m, MonadBaseControl IO m)
  => OAuth
  -> Manager
  -> AccessToken
  -> m MinimalUser
getIdCard oa manager cr = do
  res <- apiRequest oa manager cr "GET" "/v1/users/me/id_card"
  case decode (responseBody res) of
    Just a -> return a
    Nothing -> throw Mapping

-- https://dev.xing.com/docs/get/users/me/id_card
demoIdCard :: Value
demoIdCard = object [
    "id_card" .= object [
        "id"           .= ("12345_abcdef" :: BSL.ByteString)
      , "display_name" .= ("Max Mustermann" :: BSL.ByteString)
      , "permalink"    .= ("https://www.xing.com/profile/Max_Mustermann" :: BSL.ByteString)
      , "photo_urls"   .= object [
          "large"        .= ("http://www.xing.com/img/users/e/3/d/f94ef165a.123456,1.140x185.jpg" :: BSL.ByteString)
        , "mini_thumb"   .= ("http://www.xing.com/img/users/e/3/d/f94ef165a.123456,1.18x24.jpg"   :: BSL.ByteString)
        , "thumb"        .= ("http://www.xing.com/img/users/e/3/d/f94ef165a.123456,1.30x40.jpg"   :: BSL.ByteString)
        , "medium_thumb" .= ("http://www.xing.com/img/users/e/3/d/f94ef165a.123456,1.57x75.jpg"   :: BSL.ByteString)
        , "maxi_thumb"   .= ("http://www.xing.com/img/users/e/3/d/f94ef165a.123456,1.70x93.jpg"   :: BSL.ByteString)
      ]
    ]
  ]

demoIdCard' :: BSL.ByteString
demoIdCard' = encode demoIdCard
