{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Web.XING.Types.User
    (
      User(..)
    , UserId
    , PhotoUrls
    ) where

import Data.Text (Text)
import Data.Map (Map)

type UserId    = Text
type PhotoUrls = Map Text Text

class User a where
  userId      :: a -> UserId
  displayName :: a -> Text
  permalink   :: a -> Text
  photoUrls   :: a -> PhotoUrls
