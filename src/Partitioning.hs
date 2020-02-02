module Partitioning
  ( run
  ) where

import           Config             (Behavior)
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock    (UTCTime (..), secondsToDiffTime)
import           Database           (MonadDatabase)
import           Queries

run
  :: MonadDatabase m
  => Behavior
  -> m String
run behavior = do
  let t = UTCTime (fromGregorian 2020 2 1) (secondsToDiffTime 0)
  show <$> unixTimestamp t

-- createInitialPartition
  -- :: MonadDatabase m
  -- => Behavior
  -- -> MonthPartition
  -- -> m ()
-- createInitialPartition b p = 
