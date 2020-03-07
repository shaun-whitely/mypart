module Gens where

import           Hedgehog
import qualified Hedgehog.Gen   as Gen

import           Models

monthPartition :: Gen MonthPartition
monthPartition = Gen.enumBounded
