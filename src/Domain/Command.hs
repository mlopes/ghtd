module Domain.Command
  ( Command(..)
  )
where

import Domain.Action.Types

data Command
  = Add Description (Maybe Project) (Maybe Contexts)
  | Complete ActionId
  | Cancel ActionId
  | ListProjects
  | ListContexts
  | Default
  deriving (Show)

