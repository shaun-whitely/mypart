module Partitioning
  ( run
  ) where

import           App                  (HasBehavior (..))
import           AppError
import           Control.Monad.Reader (MonadReader, asks)
import           Data.List            (sort)
import           Data.Maybe           (listToMaybe)
import           Database             (MonadDatabase)
import           Models
import qualified Queries              as Q
import MonadTime
import Data.Time.Clock (UTCTime)

run
  :: ( MonadDatabase m
     , MonadReader env m
     , MonadTime m
     , HasBehavior env
     )
  => m (Either AppError ())
run = do
  maybeLatest <- getLatestPartition
  case maybeLatest of
    Nothing ->
      pure $ Left TableNotPartitioned
    Just latest -> do
      now <- currentTime
      let newParts = partitionsToCreate latest now 3
      Right <$> createPartitions newParts

partitionsToCreate
  :: MonthPartition
  -> UTCTime
  -> Months
  -> [MonthPartition]
partitionsToCreate mostRecent now plusMonths =
  let
    nowPart = utcTimeToMonthPartition now
    start = addMonths 1 mostRecent
    end = addMonths plusMonths nowPart
  in
    [start..end]

getLatestPartition
  :: ( MonadDatabase m
     , MonadReader env m
     , HasBehavior env
     )
  => m (Maybe MonthPartition)
getLatestPartition = do
  behavior <- asks getBehavior
  parts <- Q.getPartitions behavior
  pure $ findLatest parts
  where
    findLatest :: [MonthPartition] -> Maybe MonthPartition
    findLatest = listToMaybe . reverse . sort

createPartitions
  :: ( MonadDatabase m
     , MonadReader env m
     , HasBehavior env
     )
  => [MonthPartition]
  -> m ()
createPartitions ps = do
  b <- asks getBehavior
  Q.createPartitions b ps
