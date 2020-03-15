module Queries
  ( MonadQueries (..)
  ) where

import           App                   (App, behavior)
import           Config                (Behavior (..))
import           Control.Monad         (void)
import           Control.Monad.Reader  (asks)
import           Data.List             (intercalate)
import           Data.String           (IsString (..))
import           Data.Time.Clock       (UTCTime)
import           Database              (MonadDatabase (..))
import           Database.MySQL.Simple (Only (..))
import           Models
import           Text.Printf           (printf)

class Monad m => MonadQueries m where
  getPartitions :: m [MonthPartition]
  createInitialPartition :: MonthPartition -> m ()
  createPartitions :: [MonthPartition] -> m ()

instance MonadQueries App where
  getPartitions = asks behavior >>= getPartitions'
  createInitialPartition partition = asks behavior >>= createInitialPartition' partition
  createPartitions partitions = asks behavior >>= createPartitions' partitions

getPartitions'
  :: MonadDatabase m
  => Behavior
  -> m [MonthPartition]
getPartitions' b =
  let
    q = "SELECT PARTITION_NAME \
        \FROM INFORMATION_SCHEMA.PARTITIONS \
        \WHERE TABLE_SCHEMA = (SELECT DATABASE()) \
        \AND TABLE_NAME = ? \
        \AND PARTITION_NAME IS NOT NULL"
    p = Only (dbTable b)
  in
    (fmap . fmap) fromOnly $ query q p

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

createInitialPartition'
  :: MonadDatabase m
  => MonthPartition
  -> Behavior
  -> m ()
createInitialPartition' part b =
  let
    q = fromString $ printf "ALTER TABLE `%s` \
                            \PARTITION BY RANGE (`%s`) ( \
                            \PARTITION `%s` VALUES LESS THAN (UNIX_TIMESTAMP(?)) \
                            \)"
                            (dbTable b) (dbColumn b) (encodeMonthPartition part)
    p = Only (partitionEndTime part)
  in
    void $ execute q p

createPartitions'
  :: MonadDatabase m
  => [MonthPartition]
  -> Behavior
  -> m ()
createPartitions' [] _ = pure ()
createPartitions' parts b =
  let
    names = encodeMonthPartition <$> parts

    toPartitionClause :: String -> String
    toPartitionClause s = "PARTITION `" <> s <> "` VALUES LESS THAN (UNIX_TIMESTAMP(?))"

    clauses = toPartitionClause <$> names
    partFragment = intercalate ", " clauses
    q = fromString $ printf "ALTER TABLE `%s` ADD PARTITION (%s)"
                            (dbTable b) partFragment

    ps = partitionEndTime <$> parts
  in
    void $ execute q ps

partitionEndTime
  :: MonthPartition
  -> UTCTime
partitionEndTime = monthPartitionToUtcTime . addMonths 1
