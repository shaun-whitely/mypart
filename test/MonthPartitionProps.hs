{-# LANGUAGE TemplateHaskell #-}

module MonthPartitionProps where

import qualified Gens         as Gens
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import           Models

prop_encodeDecode :: Property
prop_encodeDecode =
  property $ do
    partition <- forAll $ Gens.monthPartition
    (decodeMonthPartition . encodeMonthPartition) partition === Just partition

tests :: IO Bool
tests = checkParallel $$(discover)
