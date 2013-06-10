module Web.XING
    (
      module Web.XING.Auth
    , module Web.XING.API
    , module Web.XING.Types
    -- * Calls
    , module Web.XING.Calls.IdCard
    , module Web.XING.Calls.User
    -- * Common used functions (re-exports)
    , liftIO
    , withManager
    ) where

import Web.XING.Auth
import Web.XING.API
import Web.XING.Types
import Web.XING.Calls.IdCard
import Web.XING.Calls.User

import Control.Monad.IO.Class (MonadIO (liftIO))
import Network.HTTP.Conduit (withManager)
