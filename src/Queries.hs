module Queries
  ( getPartitions
  , unixTimestamp
  ) where

import           Config                       (Behavior (..))
import           Data.Time.Clock              (UTCTime)
import           Database                     (MonadDatabase (..))
import           Database.MySQL.Simple        (Only (..))
import           Models

getPartitions
  :: ( MonadDatabase m
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

unixTimestamp
  :: MonadDatabase m
  => UTCTime
  -> m Int
unixTimestamp t =
  let
    q = "SELECT UNIX_TIMESTAMP(?)"
    p = Only t
  in
    (fromOnly . head) <$> query q p

-- alterTablePartitionByRange
  -- :: ( MonadDatabase m
     -- , Param a
  -- => Behavior
  -- -> String -- partition name
  -- ->
