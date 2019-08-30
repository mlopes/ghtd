module Domain.Action.Types
  ( Action(..)
  , Actions
  , ActionState(..)
  , ActionId
  , Description
  , ActionsModifier
  , Project
  , Projects
  , Context
  , Contexts
  ) where

data Action =
  Action ActionId Description Project Contexts ActionState

data ActionState = ToDo
                 | Done
                 | Cancelled
                 deriving Eq

type Actions = [Action]

type ActionId = Text

type Description = Text

type ActionsModifier = Actions -> ActionId -> Actions

type Project = Text

type Projects = [Project]

type Context = Text

type Contexts = [Context]

