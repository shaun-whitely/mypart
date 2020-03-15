module Partitioning
  ( run
  ) where

import           AppError
import           Data.List       (sort)
import           Data.Maybe      (listToMaybe)
import           Data.Time.Clock (UTCTime)
import           Models
import           MonadTime
import qualified Queries         as Q

run
  :: ( Q.MonadQueries m
     , MonadTime m
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
      Right <$> Q.createPartitions newParts

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
  :: Q.MonadQueries m
  => m (Maybe MonthPartition)
getLatestPartition = findLatest <$> Q.getPartitions
  where
    findLatest :: [MonthPartition] -> Maybe MonthPartition
    findLatest = listToMaybe . reverse . sort
