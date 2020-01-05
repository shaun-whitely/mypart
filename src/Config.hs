module Config
  ( Config(..)
  , fromEnvironment
  ) where

import           Data.Text                      ( Text
                                                )
import qualified Data.Text                      as T
import           System.Environment             ( getEnv
                                                )
    
data Config = Config
  { dbHost :: Text
  , dbPort :: Text
  , dbUser :: Text
  , dbPassword :: Text
  , dbTable :: Text
  , dbColumn :: Text
  }

fromEnvironment :: IO Config
fromEnvironment =
  Config
  <$> get "DB_HOST"
  <*> get "DB_PORT"
  <*> get "DB_USER"
  <*> get "DB_PASSWORD"
  <*> get "DB_TABLE"
  <*> get "DB_COLUMN"
  where
    get s = T.pack <$> getEnv s
