{-# LANGUAGE NoImplicitPrelude #-}

module Action
  ( Action(..)
  , ActionState(..)
  )
where

import           ClassyPrelude

type ActionId = Text
type Description = Text
type Project = Text
type Contexts = [Text]

data ActionState = ToDo
                 | Done
                 | Cancelled
                 deriving Eq

data Action =
  Action ActionId Description Project Contexts ActionState

