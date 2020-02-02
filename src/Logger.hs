module Logger
  ( MonadLogger (..)
  , logInfo
  , logWarn
  , logError
  ) where

import           App                    (App)
import           Control.Monad.IO.Class (liftIO)
import           Text.Printf            (printf)

data LogLevel
  = Info
  | Warn
  | Error

formatLogLevel :: LogLevel -> String
formatLogLevel Info  = "INFO"
formatLogLevel Warn  = "WARN"
formatLogLevel Error = "ERROR"

class Monad m => MonadLogger m where
  logS :: LogLevel -> String -> m ()

logInfo :: MonadLogger m => String -> m ()
logInfo = logS Info

logWarn :: MonadLogger m => String -> m ()
logWarn = logS Warn

logError :: MonadLogger m => String -> m ()
logError = logS Error

instance MonadLogger App where
  logS level msg = liftIO $ printf "[%s]: %s" (formatLogLevel level) msg
