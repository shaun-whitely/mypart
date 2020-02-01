module Partitioning
  ( run
  ) where

import           Config   (Behavior)
import           Database (MonadDatabase)
import           Queries

run
  :: MonadDatabase m
  => Behavior
  -> m String
run behavior = show <$> getPartitions @_ @Int behavior
