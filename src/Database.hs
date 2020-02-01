module Database
  ( MonadDatabase(..)
  ) where

import           App
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Reader               (asks)
import           Data.Int                           (Int64)
import           Database.MySQL.Simple              (Query)
import qualified Database.MySQL.Simple              as SQL
import           Database.MySQL.Simple.QueryParams  (QueryParams)
import           Database.MySQL.Simple.QueryResults (QueryResults)

class Monad m => MonadDatabase m where
  query :: (QueryParams q, QueryResults r) => Query -> q -> m [r]
  query_ :: QueryResults r => Query -> m [r]
  execute :: QueryParams q => Query -> q -> m Int64
  execute_ :: Query -> m Int64

instance MonadDatabase App where
  query q params = do
    conn <- asks dbConnection
    liftIO $ SQL.query conn q params

  query_ q = do
    conn <- asks dbConnection
    liftIO $ SQL.query_ conn q

  execute q params = do
    conn <- asks dbConnection
    liftIO $ SQL.execute conn q params

  execute_ q = do
    conn <- asks dbConnection
    liftIO $ SQL.execute_ conn q
