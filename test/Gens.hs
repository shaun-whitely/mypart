module Gens where

import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

import           Models

monthPartition :: Gen MonthPartition
monthPartition =
  Gen.just $ mkMonthPartition <$> years <*> months

years :: Gen Years
years = Gen.int Range.linearBounded

months :: Gen Months
months = Gen.int $ Range.linear 1 12
