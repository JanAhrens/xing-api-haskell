{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Web.XING.Calls.User
    (
        demoUser
      , demoUser'
      , demoUsers
      , demoUsers'
      , getUser
      , User(..)
    ) where

import Web.XING.Types
import Web.XING.API
import Data.Aeson ( encode, decode, Value(..)
                  , object, (.=) )
import qualified Data.ByteString.Lazy.Char8 as BSL
import Network.HTTP.Conduit (Response(..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Monoid (mappend)
import Control.Exception (throw)
import Data.Text.Encoding (encodeUtf8)
import Data.Text (Text)

getUser
  :: (MonadResource m, MonadBaseControl IO m)
  => OAuth
  -> Manager
  -> AccessToken
  -> Text
  -> m FullUser
getUser oa manager cr uid = do
  Response _ _ _ body <- apiRequest oa manager cr "GET" ("/v1/users/me/" `mappend` encodeUtf8 uid)
  case decode body of
    Just a -> return a
    Nothing -> throw Mapping

-- https://dev.xing.com/docs/get/users/:id
demoUsers :: Value
demoUsers = object [
    "users" .= [demoUser]
  ]

demoUsers' :: BSL.ByteString
demoUsers' = encode demoUsers

demoUser :: Value
demoUser = object [
        "id"           .= ("12345_abcdef" :: BSL.ByteString)
      , "first_name"   .= ("Max" :: BSL.ByteString)
      , "last_name"    .= ("Mustermann" :: BSL.ByteString)
      , "display_name" .= ("Max Mustermann" :: BSL.ByteString)
      , "page_name"    .= ("Max_Mustermann" :: BSL.ByteString)
      , "permalink"    .= ("https://www.xing.com/profile/Max_Mustermann" :: BSL.ByteString)
      , "gender"       .= ("m" :: BSL.ByteString)
      , "birth_date"   .= object [
          "day"   .= (12 :: Int)
        , "month" .= (8 :: Int)
        , "year"  .= (1963 :: Int)
      ]
      , "active_email" .= ("max.mustermann@xing.com" :: BSL.ByteString)
      , "time_zone" .= object [
          "name"       .= ("Europe/Copenhagen" :: BSL.ByteString)
        , "utc_offset" .= (2.0 :: Float)
      ]
      , "badges"    .= (["PREMIUM", "PRIVATEMESSAGES"] :: [BSL.ByteString])
      , "wants"     .= (encodeUtf8 "einen neuen Job")
      , "haves"     .= (encodeUtf8 "viele tolle Skills")
      , "interests" .= (encodeUtf8 "Flitzebogen schießen and so on")
      , "organisation_member" .= (encodeUtf8 "ACM, GI")
      , "languages" .= object [
          "de" .= ("NATIVE" :: BSL.ByteString)
        , "en" .= ("FLUENT" :: BSL.ByteString)
        , "fr" .= Null
        , "zh" .= ("BASIC" :: BSL.ByteString)
      ]
      , "private_address" .= object [
          "city"         .= (encodeUtf8 "Hamburg")
        , "country"      .= (encodeUtf8 "DE")
        , "zip_code"     .= (encodeUtf8 "20357")
        , "street"       .= (encodeUtf8 "Privatstraße 1")
        , "phone"        .= (encodeUtf8 "49|40|1234560")
        , "fax"          .= (encodeUtf8 "||")
        , "province"     .= (encodeUtf8 "Hamburg")
        , "email"        .= (encodeUtf8 "max@mustermann.de")
        , "mobile_phone" .= (encodeUtf8 "49|0155|1234567")
      ]
      , "business_address" .= object [
          "city"         .= (encodeUtf8 "Hamburg")
        , "country"      .= (encodeUtf8 "DE")
        , "zip_code"     .= (encodeUtf8 "20357")
        , "street"       .= (encodeUtf8 "Geschäftsstraße 1a")
        , "phone"        .= (encodeUtf8 "49|40|1234569")
        , "fax"          .= (encodeUtf8 "49|40|1234561")
        , "province"     .= (encodeUtf8 "Hamburg")
        , "email"        .= (encodeUtf8 "max.mustermann@xing.com")
        , "mobile_phone" .= (encodeUtf8 "49|160|66666661")
      ]
      , "web_profiles" .= object [
          "qype"        .= (["http://qype.de/users/foo"] :: [BSL.ByteString])
        , "google_plus" .= (["http://plus.google.com/foo"] :: [BSL.ByteString])
        , "blog"        .= (["http://blog.example.org"] :: [BSL.ByteString])
        , "homepage"    .= (["http://example.org", "http://another-example.org"] :: [BSL.ByteString])
      ]
      , "instant_messaging_accounts" .= object [
          "skype"      .= (encodeUtf8 "1122334455")
        , "googletalk" .= (encodeUtf8 "max.mustermann")
      ]
      , "professional_experience" .= object [
          "primary_company" .= object [
              "name"         .= (encodeUtf8 "XING AG")
            , "title"        .= (encodeUtf8 "Softwareentwickler")
            , "company_size" .= (encodeUtf8 "201-500")
            , "tag"          .= Null
            , "url"          .= (encodeUtf8 "http://www.xing.com")
            , "career_level" .= (encodeUtf8 "PROFESSIONAL_EXPERIENCED")
            , "begin_date"   .= (encodeUtf8 "2010-01")
            , "description"  .= Null
            , "end_date"     .= Null
            , "industry"     .= (encodeUtf8 "AEROSPACE")
        ]
        , "non_primary_companies" .= [
            object [
              "name"         .= (encodeUtf8 "Ninja Ltd.")
            , "title"        .= (encodeUtf8 "DevOps")
            , "company_size" .= Null
            , "tag"          .= (encodeUtf8 "NINJA")
            , "url"          .= (encodeUtf8 "http://www.ninja-ltd.co.uk")
            , "career_level" .= Null
            , "begin_date"   .= (encodeUtf8 "2009-04")
            , "description"  .= Null
            , "end_date"     .= (encodeUtf8 "2010-07")
            , "industry"     .= (encodeUtf8 "ALTERNATIVE_MEDICINE")
          ]
          , object [
              "name"         .= Null
            , "title"        .= (encodeUtf8 "Wiss. Mitarbeiter")
            , "company_size" .= Null
            , "tag"          .= (encodeUtf8 "OFFIS")
            , "url"          .= (encodeUtf8 "http://www.uni.de")
            , "career_level" .= Null
            , "begin_date"   .= (encodeUtf8 "2007")
            , "description"  .= Null
            , "end_date"     .= (encodeUtf8 "2008")
            , "industry"     .= (encodeUtf8 "APPAREL_AND_FASHION")
          ]
          , object [
              "name"         .= Null
            , "title"        .= (encodeUtf8 "TEST NINJA")
            , "company_size" .= (encodeUtf8 "201-500")
            , "tag"          .= (encodeUtf8 "TESTCOMPANY")
            , "url"          .= Null
            , "career_level" .= (encodeUtf8 "ENTRY_LEVEL")
            , "begin_date"   .= (encodeUtf8 "1998-12")
            , "description"  .= Null
            , "end_date"     .= (encodeUtf8 "1999-05")
            , "industry"     .= (encodeUtf8 "ARTS_AND_CRAFTS")
          ]
        ]
        , "awards" .= [
          object [
              "name"         .= (encodeUtf8 "Awesome Dude Of The Year")
            , "date_awarded" .= (2007 :: Int)
            , "url"          .= Null
          ]
        ]
      ]
      , "educational_background" .= object [
          "schools" .= [
            object [
              "name"    .= (encodeUtf8 "Carl-von-Ossietzky Universtät Schellenburg")
            , "degree"  .= (encodeUtf8 "MSc CE/CS")
            , "notes"   .= Null
            , "subject" .= Null
            , "begin_date" .= (encodeUtf8 "1998-08")
            , "end_date"   .= (encodeUtf8 "2005-02")
            ]
          ]
        , "qualifications" .= (["TOEFLS", "PADI AOWD"] :: [BSL.ByteString])
      ]
      , "photo_urls"   .= object [
          "large"        .= ("http://www.xing.com/img/users/e/3/d/f94ef165a.123456,1.140x185.jpg" :: BSL.ByteString)
        , "mini_thumb"   .= ("http://www.xing.com/img/users/e/3/d/f94ef165a.123456,1.18x24.jpg"   :: BSL.ByteString)
        , "thumb"        .= ("http://www.xing.com/img/users/e/3/d/f94ef165a.123456,1.30x40.jpg"   :: BSL.ByteString)
        , "medium_thumb" .= ("http://www.xing.com/img/users/e/3/d/f94ef165a.123456,1.57x75.jpg"   :: BSL.ByteString)
        , "maxi_thumb"   .= ("http://www.xing.com/img/users/e/3/d/f94ef165a.123456,1.70x93.jpg"   :: BSL.ByteString)
      ]
    ]

demoUser' :: BSL.ByteString
demoUser' = encode demoUser
