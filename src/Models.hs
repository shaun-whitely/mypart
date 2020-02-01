module Models
  ( Partition(..)
  , IntPartition
  ) where

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

instance (Result a, Read a) => QueryResults (Partition a) where
  convertResults fields values = Partition n vlt
    where
      (n, vltStr) = convertResults fields values :: (String, String)
      vlt         = case readMaybe vltStr of
                      Just s  -> s
                      Nothing -> convertError fields values 2
