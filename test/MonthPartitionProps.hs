{-# LANGUAGE TemplateHaskell #-}

module MonthPartitionProps where

import qualified Gens         as Gens
import           Hedgehog
import           Models

prop_encodeDecode :: Property
prop_encodeDecode =
  property $ do
    partition <- forAll $ Gens.monthPartition
    (decodeMonthPartition . encodeMonthPartition) partition === Just partition

prop_toFromUtcTime :: Property
prop_toFromUtcTime =
  property $ do
    partition <- forAll $ Gens.monthPartition
    (utcTimeToMonthPartition . monthPartitionToUtcTime) partition === partition

tests :: IO Bool
tests = checkParallel $$(discover)
