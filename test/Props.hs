module Props where

import           Hedgehog
import           Hedgehog.Main
import qualified MonthPartitionProps

main = defaultMain [MonthPartitionProps.tests]
