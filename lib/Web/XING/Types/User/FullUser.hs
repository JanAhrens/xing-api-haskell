{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Web.XING.Types.User.FullUser
    (
      FullUser
    , UserList(..)
    , Gender(..)
    , Language, Skill
    --
    , birthDate, gender, firstName, lastName
    , activeEmail, premiumServices, badges, languages
    , wants, haves, interests, organisations, pageName
    , privateAddress, businessAddress
    ) where

import Web.XING.Types.User
import Web.XING.Types.BirthDate
import Web.XING.Types.Address

import Data.Aeson (Value(..), FromJSON(..), (.:), (.:?))
import Data.Aeson.Types (parseMaybe)
import Control.Monad (mzero)
import Data.Text (Text)
import Data.Time.LocalTime (TimeZone(..))
import Data.Map (Map)
import Control.Applicative ((<$>), (<*>))

type Language = Text
type Skill = Text

data Gender
  = Male
  | Female
  deriving (Eq, Show)

instance FromJSON Gender where
  parseJSON (String "m") = return Male
  parseJSON (String "f") = return Female
  parseJSON _            = mzero

newtype UserList = UserList { unUserList :: [FullUser] }
  deriving (Show)

-- TODO: it would be nice, if instead of using the UserList hack, we could use:
--   instance FromJSON [FullUser] where
instance FromJSON UserList where
  parseJSON (Object response) = do
    users <- parseJSON =<< (response .: "users")
    return $ UserList users
  parseJSON _ = mzero

data FullUser
  = FullUser {
      _userId          :: UserId
    , _displayName     :: Text
    , _permalink       :: Text
    , _firstName       :: Text
    , _lastName        :: Text
    , _pageName        :: Text
    , _gender          :: Gender
    , _activeEmail     :: Maybe Text
    , _timeZone        :: TimeZone
    , _premiumServices :: [Text]
    , _badges          :: [Text]
    , _languages       :: Map Language (Maybe Skill)
    , _wants           :: Maybe Text
    , _haves           :: Maybe Text
    , _interests       :: Maybe Text
    , _organisations   :: Maybe Text
    , _privateAddress  :: Address
    , _businessAddress :: Address
    , _photoUrls       :: PhotoUrls
    , _birthDate       :: Maybe BirthDate
  }
  deriving (Show, Eq)

instance User FullUser where
  userId      = _userId
  displayName = _displayName
  permalink   = _permalink
  photoUrls   = _photoUrls

instance FromJSON FullUser where
  parseJSON (Object response) = do
    FullUser <$> (response .: "id")
             <*> (response .: "display_name")
             <*> (response .: "permalink")
             <*> (response .: "first_name")
             <*> (response .: "last_name")
             <*> (response .: "page_name")
             <*> (parseJSON =<< response .: "gender")
             <*> (response .:? "active_email")
             <*> (response .: "time_zone" >>= \zone -> do
                    TimeZone <$> (return . (60 *) =<< zone .: "utc_offset")
                             <*> return False
                             <*> (zone .: "name"))
             <*> (response .: "premium_services")
             <*> (response .: "badges")
             <*> (response .: "languages")
             <*> (response .: "wants")
             <*> (response .: "haves")
             <*> (response .: "interests")
             <*> (response .: "organisation_member")
             <*> (response .: "private_address")
             <*> (response .: "business_address")
             <*> (response .: "photo_urls")
             <*> (return . (parseMaybe parseJSON) =<< response .: "birth_date")
  parseJSON _ = mzero

birthDate
  :: FullUser
  -> Maybe BirthDate
birthDate = _birthDate

gender
  :: FullUser
  -> Gender
gender = _gender

firstName, lastName
  :: FullUser
  -> Text
firstName = _firstName
lastName  = _lastName

activeEmail
  :: FullUser
  -> Maybe Text
activeEmail = _activeEmail

premiumServices, badges
  :: FullUser
  -> [Text]
premiumServices = _premiumServices
badges          = _badges

languages
  :: FullUser
  -> Map Language (Maybe Skill)
languages = _languages

wants, haves, interests, organisations
  :: FullUser
  -> Maybe Text
wants         = _wants
haves         = _haves
interests     = _interests
organisations = _organisations

pageName
  :: FullUser
  -> Text
pageName = _pageName

privateAddress, businessAddress
  :: FullUser
  -> Address
privateAddress  = _privateAddress
businessAddress = _businessAddress
