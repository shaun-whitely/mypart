module Config
  ( Config(..)
  , Behavior(..)
  , fromEnvironment
  ) where

import           Data.Word          (Word16)
import           System.Environment (getEnv)

data Behavior
  = Behavior
  { dbTable  :: String
  , dbColumn :: String
  , partitionsAhead :: Int
  }

data Config
  = Config
  { dbHost     :: String
  , dbPort     :: Word16
  , dbUser     :: String
  , dbPassword :: String
  , dbDatabase :: String
  , dbBehavior :: Behavior
  }

fromEnvironment :: IO Config
fromEnvironment =
  Config
  <$> get "DB_HOST"
  <*> getA "DB_PORT"
  <*> get "DB_USER"
  <*> get "DB_PASSWORD"
  <*> get "DB_DATABASE"
  <*> behavior
  where
    get s = getEnv s
    getA s = read <$> getEnv s
    behavior =
      Behavior
      <$> get "DB_TABLE"
      <*> get "DB_COLUMN"
      <*> getA "PARTITIONS_AHEAD"
