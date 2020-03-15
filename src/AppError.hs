module AppError
  ( AppError (..)
  ) where

data AppError
  = TableNotPartitioned
  deriving (Eq, Show)
