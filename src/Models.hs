module Models
  ( MonthPartition (..)
  , decodeMonthPartition
  , encodeMonthPartition
  , monthPartitionToUtcTime
  ) where

import           Data.List.Split                    (splitOn)
import           Data.Time.Calendar                 (fromGregorian)
import           Data.Time.Clock                    (UTCTime (..),
                                                     secondsToDiffTime)
import           Database.MySQL.Simple.QueryResults (convertError)
import           Database.MySQL.Simple.Result       (Result (..))
import           Text.Printf                        (printf)
import           Text.Read                          (readMaybe)

data MonthPartition
  = MonthPartition
  { year  :: Integer
  , month :: Int
  }
  deriving Show

decodeMonthPartition :: String -> Maybe MonthPartition
decodeMonthPartition s =
  case (splitOn "_" s) of
    [y, m] -> MonthPartition <$> readMaybe y <*> readMaybe m
    _      -> Nothing

encodeMonthPartition :: MonthPartition -> String
encodeMonthPartition (MonthPartition y m) =
  printf "%d_%02d" y m

monthPartitionToUtcTime :: MonthPartition -> UTCTime
monthPartitionToUtcTime (MonthPartition y m) =
  UTCTime (fromGregorian y m 1) (secondsToDiffTime 0)

instance Result MonthPartition where
  convert f v =
    case decodeMonthPartition str of
      Just p  -> p
      Nothing -> convertError [f] [v] 1
    where
      str :: String
      str = convert f v
