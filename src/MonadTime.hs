module MonadTime
  ( MonadTime (..)
  ) where

import           App                    (App)
import           Control.Monad.IO.Class (liftIO)

import qualified Data.Time.Clock        as C

class Monad m => MonadTime m where
  currentTime :: m C.UTCTime

instance MonadTime App where
  currentTime = liftIO C.getCurrentTime
