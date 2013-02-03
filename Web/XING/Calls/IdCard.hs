{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module Web.XING.Calls.IdCard(
    getIdCard
  , IdCard(..)
  , demoIdCard
  , demoIdCard'
) where

import Web.XING
import Web.XING.Auth
import qualified Data.ByteString.Lazy.Char8 as BSL
import Control.Monad.Trans.Resource (MonadResource, MonadBaseControl)
import Control.Applicative ((<$>), (<*>))
import Data.Aeson (
    (.:), decode, FromJSON(..), Value(..)
  , object, (.=), encode
  )
import Data.HashMap.Lazy (HashMap)
import Network.HTTP.Conduit (Response(..))

getIdCard :: (MonadResource m, MonadBaseControl IO m)
          => OAuth
          -> Manager
          -> Credential
          -> m (Maybe IdCard)
getIdCard oa manager cr = do
  Response _ _ _ body <- apiRequest oa manager cr "GET" "/v1/users/me/id_card"
  return $ decode body

data IdCard = IdCard {
    id          :: BSL.ByteString
  , displayName :: BSL.ByteString
  , permalink   :: BSL.ByteString
  , photoUrls   :: HashMap BSL.ByteString BSL.ByteString
} deriving (Show, Eq)

instance FromJSON IdCard where
  parseJSON (Object response) = do
    container <- response .: "id_card"
    IdCard <$> (container .: "id")
           <*> (container .: "display_name")
           <*> (container .: "permalink")
           <*> (container .: "photo_urls")
  parseJSON _ = fail "no parse"

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
