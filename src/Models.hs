module Models
  ( MonthPartition
  , addMonths
  , decodeMonthPartition
  , encodeMonthPartition
  , mkMonthPartition
  , monthPartitionToUtcTime
  , utcTimeToMonthPartition
  , Years
  , Months
  ) where

import           Data.List.Split                    (splitOn)
import           Data.Time.Calendar                 (fromGregorian, toGregorian)
import           Data.Time.Clock                    (UTCTime (..),
                                                     secondsToDiffTime)
import           Database.MySQL.Simple.QueryResults (convertError)
import           Database.MySQL.Simple.Result       (Result (..))
import           Text.Printf                        (printf)
import           Text.Read                          (readMaybe)

type Years = Int
type Months = Int

data MonthPartition
  = MonthPartition
  { year  :: Years
  , month :: Months
  }
  deriving (Eq, Show)

mkMonthPartition :: Years -> Months -> Maybe MonthPartition
mkMonthPartition y m = MonthPartition y <$> getMonth m
  where
    getMonth m = if (m >= 1) && (m <= 12) then Just m else Nothing

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
  UTCTime (fromGregorian (toInteger y) m 1) (secondsToDiffTime 0)

utcTimeToMonthPartition :: UTCTime -> MonthPartition
utcTimeToMonthPartition t =
  let
    (y, m, d) = toGregorian (utctDay t)
  in
    MonthPartition (fromInteger y) m

addMonths :: Months -> MonthPartition -> MonthPartition
addMonths i =
  toEnum . (+i) . fromEnum


instance Result MonthPartition where
  convert f v =
    case decodeMonthPartition str of
      Just p  -> p
      Nothing -> convertError [f] [v] 1
    where
      str :: String
      str = convert f v

instance Enum MonthPartition where
  toEnum i = MonthPartition (i `div` 12) ((i `mod` 12) + 1)
  fromEnum (MonthPartition y m) = y * 12 + (m - 1)

instance Ord MonthPartition where
  compare a b = compare (fromEnum a) (fromEnum b)
