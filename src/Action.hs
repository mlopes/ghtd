{-# LANGUAGE NoImplicitPrelude #-}

module Action
  ( Action(..)
  , ActionState(..)
  )
where

import           Data.Text.Lazy                 ( Text )

type ActionId = Text
type Description = Text
type Project = Text
type Contexts = [Text]

data ActionState = ToDo
                 | Done
                 | Cancelled

data Action =
  Action ActionId Description Project Contexts ActionState

