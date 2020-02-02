module Models
  ( Partition(..)
  , IntPartition
  , MonthPartition (..)
  , decodeMonthPartition
  , encodeMonthPartition
  , monthPartitionToUtcTime
  ) where

import           Data.List.Split                    (splitOn)
import           Data.Time.Calendar                 (fromGregorian)
import           Data.Time.Clock                    (UTCTime (..),
                                                     secondsToDiffTime)
import           Database.MySQL.Simple.QueryResults (QueryResults (..),
                                                     convertError)
import           Database.MySQL.Simple.Result       (Result)
import           Text.Read                          (readMaybe)

data Partition a
  = Partition
  { name           :: String
  , valuesLessThan :: a
  }
  deriving Show

type IntPartition = Partition Int

instance (Read a) => QueryResults (Partition a) where
  convertResults fields values = Partition n vlt
    where
      (n, vltStr) = convertResults fields values :: (String, String)
      vlt         = case readMaybe vltStr of
                      Just s  -> s
                      Nothing -> convertError fields values 2

data MonthPartition
  = MonthPartition
  { year  :: Integer
  , month :: Int
  }

decodeMonthPartition :: String -> Maybe MonthPartition
decodeMonthPartition s =
  case (splitOn "_" s) of
    [y, m] -> MonthPartition <$> readMaybe y <*> readMaybe m
    _      -> Nothing

encodeMonthPartition :: MonthPartition -> String
encodeMonthPartition (MonthPartition y m) = (show y) <> "_" <> (show m)

monthPartitionToUtcTime :: MonthPartition -> UTCTime
monthPartitionToUtcTime (MonthPartition y m) =
  UTCTime (fromGregorian y m 1) (secondsToDiffTime 0)
