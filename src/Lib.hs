module Lib
  ( run
  ) where

import           App
import           Config
import           Control.Exception      (bracket)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (MonadReader, asks, runReaderT)
import           Data.Maybe             (listToMaybe)
import           Database
import qualified Database.MySQL.Simple  as SQL
import qualified Partitioning

withDbConnection :: Config -> (SQL.Connection -> IO a) -> IO a
withDbConnection config f = bracket c SQL.close f
  where
    c = SQL.connect info
    info =
      SQL.defaultConnectInfo
      { SQL.connectHost = dbHost config
      , SQL.connectPort = dbPort config
      , SQL.connectUser = dbUser config
      , SQL.connectPassword = dbPassword config
      , SQL.connectDatabase = dbDatabase config
      }

run :: IO ()
run = do
  config <- fromEnvironment
  withDbConnection config $ \c ->
    let
      env = Env { behavior = dbBehavior config, dbConnection = c }
    in
      runReaderT (runApp app) env

app :: App ()
app = do
  b <- asks getBehavior
  result <- Partitioning.run b
  liftIO $ putStrLn result
