module Props where

import           Hedgehog.Main
import qualified MonthPartitionProps
import qualified Laws

main :: IO ()
main = defaultMain [ MonthPartitionProps.tests
                   , Laws.tests
                   ]
