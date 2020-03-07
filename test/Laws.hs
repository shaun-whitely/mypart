module Laws where

import Hedgehog.Classes
import qualified Gens

laws :: [(String, [Laws])]
laws =
  [ ("MonthPartition", [ boundedEnumLaws
                       , ordLaws
                       ] <*> pure Gens.monthPartition)
  ]

tests :: IO Bool
tests = lawsCheckMany laws
