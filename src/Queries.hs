module Queries
  ( getPartitions
  ) where

import           Config                       (Behavior (..))
import           Database                     (MonadDatabase (..))
import           Database.MySQL.Simple.Result (Result)
import           Models

getPartitions
  :: ( MonadDatabase m
     , Result a
     , Read a
     )
  => Behavior
  -> m [Partition a]
getPartitions behavior =
  let
    q = "SELECT PARTITION_NAME, PARTITION_DESCRIPTION \
        \FROM INFORMATION_SCHEMA.PARTITIONS \
        \WHERE TABLE_SCHEMA = (SELECT DATABASE()) \
        \AND TABLE_NAME = ? \
        \AND PARTITION_EXPRESSION = ?"
    params = (dbTable behavior, dbColumn behavior)
  in
    query q params
