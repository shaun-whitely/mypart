{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module App
  ( App (..)
  , Env (..)
  , HasBehavior (..)
  ) where

import           Config                 (Behavior)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, ReaderT)
import           Database.MySQL.Simple  (Connection)

data Env
  = Env
  { behavior     :: Behavior
  , dbConnection :: Connection
  }

newtype App a
  = App
  { runApp :: ReaderT Env IO a
  }
  deriving
  ( Functor
  , Applicative
  , Monad
  , MonadReader Env
  , MonadIO
  )

class HasBehavior a where
  getBehavior :: a -> Behavior

instance HasBehavior Env where
  getBehavior = behavior

instance HasBehavior Behavior where
  getBehavior = id
