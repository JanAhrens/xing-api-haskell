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
    ) where

import Web.XING.Types.User
import Web.XING.Types.BirthDate
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
      _userId      :: UserId
    , _displayName :: Text
    , _permalink   :: Text
    , _photoUrls   :: PhotoUrls
    , _gender      :: Gender
    , _firstName   :: Text
    , _lastName    :: Text
    , _activeEmail :: Maybe Text
    , _timeZone    :: TimeZone
    , _premiumServices :: [Text]
    , _badges      :: [Text]
    , _languages   :: Map Language (Maybe Skill)
    , _birthDate   :: Maybe BirthDate
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
             <*> (response .: "photo_urls")
             <*> (parseJSON =<< response .: "gender")
             <*> (response .: "first_name")
             <*> (response .: "last_name")
             <*> (response .:? "active_email")
             <*> (response .: "time_zone" >>= \zone -> do
                    TimeZone <$> (return . (60 *) =<< zone .: "utc_offset")
                             <*> return False
                             <*> (zone .: "name"))
             <*> (response .: "premium_services")
             <*> (response .: "badges")
             <*> (response .: "languages")
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

firstName
  :: FullUser
  -> Text
firstName = _firstName

lastName
  :: FullUser
  -> Text
lastName = _lastName

activeEmail
  :: FullUser
  -> Maybe Text
activeEmail = _activeEmail

premiumServices
  :: FullUser
  -> [Text]
premiumServices = _premiumServices

badges
  :: FullUser
  -> [Text]
badges = _badges

languages
  :: FullUser
  -> Map Language (Maybe Skill)
languages = _languages
