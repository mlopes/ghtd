module Command.Command
  ( Command(..)
  ) where

data Command
  = Add
  | Remove
  | Projects
  | Contexts
  | Help
  deriving (Show)
