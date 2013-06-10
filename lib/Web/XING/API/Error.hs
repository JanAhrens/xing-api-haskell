{-# LANGUAGE OverloadedStrings #-}

module Web.XING.API.Error
    (
      mapError
    , handleError
    , handleStatusCodeException
    ) where

import Web.XING.Types
import Network.HTTP.Types (ResponseHeaders)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
import Control.Applicative ((<$>), (<*>))
import Data.Aeson (decode, FromJSON(parseJSON), Value(Object) , (.:))
import Control.Exception (throw)
import Network.HTTP.Conduit (HttpException(StatusCodeException))

mapError
  :: XINGError
  -> APIError
mapError (XINGError "INVALID_OAUTH_SIGNATURE" _)        = OAuthError "Invalid oauth signature. Check your consumer and access token secret."
mapError (XINGError "INVALID_OAUTH_CONSUMER" _)         = OAuthError "Invalid oauth consumer key. Check your config."
mapError (XINGError "INSUFFICIENT_PRIVILEGES" _)        = OAuthError "Required permission missing"
mapError (XINGError "INVALID_OAUTH_VERSION" _)          = OAuthError "Invalid oauth version"
mapError (XINGError "INVALID_OAUTH_SIGNATURE_METHOD" _) = OAuthError "Invalid oauth version"
mapError (XINGError "REQUIRED_PARAMETER_MISSING" _)     = OAuthError "Required parameter missing. Should never have happened."
mapError (XINGError "INVALID_REQUEST_TOKEN" _)          = OAuthError "Invalid request token. Maybe the verifier was wrong?"
mapError (XINGError "INVALID_OAUTH_TOKEN" _)            = TokenError "The oauth token was invalid. Maybe the user revoked it?"
mapError (XINGError "TIME_EXPIRED" _)                   = OAuthError "Time was too old. Make sure that your local clock is set correctly."
mapError (XINGError "RATE_LIMIT_EXCEEDED" _)            = Throttled
mapError (XINGError "CALLBACK_URL_NOT_ALLOWED" _)       = CallError "The callback URL was not allowed" -- TOOD API shows better error message
mapError _                                              = CallError "unknown"

data XINGError =
  XINGError BSL.ByteString BSL.ByteString
  deriving (Show, Eq)

instance FromJSON XINGError where
  parseJSON (Object response) =
    XINGError <$> (response .: "error_name")
              <*> (response .: "message")
  parseJSON _ = fail "no parse"

handleStatusCodeException
  :: BS.ByteString
  -> HttpException
  -> a
handleStatusCodeException call (StatusCodeException status headers) = throw $ handleError status headers call
handleStatusCodeException _    e                                    = throw e

handleError
  :: Status
  -> ResponseHeaders
  -> BS.ByteString
  -> APIError
handleError _ headers _ =
  case decode $ BSL.fromChunks [fromMaybe "{}" (lookup "X-Response-Body-Start" headers)] of
    Just e  -> mapError e
    Nothing -> CallError "unknown"
