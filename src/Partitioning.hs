module Partitioning
  ( run
  ) where

import           Config             (Behavior)
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock    (UTCTime (..), secondsToDiffTime)
import           Database           (MonadDatabase)
import           Models
import           Queries

run
  :: MonadDatabase m
  => Behavior
  -> m String
run behavior = do
  createInitialPartition behavior (MonthPartition 2020 1)
  createPartitions
    behavior
    [ MonthPartition 2020 2
    , MonthPartition 2020 3
    , MonthPartition 2020 4
    ]
  show <$> getPartitions behavior
