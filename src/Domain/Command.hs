module Domain.Command
  ( Command(..)
  )
where

import Domain.Action

data Command
  = Add Description Project Contexts
  | Complete ActionId
  | Cancel ActionId
  | Default
  deriving (Show)

