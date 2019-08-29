{-# LANGUAGE OverloadedStrings #-}

module Domain.Action
  ( Action(..)
  , ActionState(..)
  , ActionId
  , Description
  , ActionsModifier
  , Actions
  , addAction
  , completeAction
  , cancelAction
  , projectsFromActions
  , contextsOfActions
  , module Domain.Contexts
  , module Domain.Project
  )
where

import Data.UUID
import Data.List (nub)

import Domain.Contexts
import Domain.Project

type ActionId = Text
type Description = Text

type ActionsModifier = Actions -> ActionId -> Actions

data ActionState = ToDo
                 | Done
                 | Cancelled
                 deriving Eq

data Action =
  Action ActionId Description Project Contexts ActionState

type Actions = [Action]

addAction :: Actions -> UUID -> Description -> Project -> Contexts -> (Action, Actions)
addAction as a d p c =
  (action, action : as)
  where
    action = Action (textNoDashesUUID4 a) d p c ToDo
    textNoDashesUUID4 :: UUID -> Text
    textNoDashesUUID4 = stripDashes . toText
    stripDashes :: Text -> Text
    stripDashes = filter ('-' /=)

completeAction :: ActionsModifier
completeAction actions aId = fmap (\x -> modifyState aId x Done) actions

cancelAction :: ActionsModifier
cancelAction actions aId = fmap (\x -> modifyState aId x Cancelled) actions

projectsFromActions :: Actions -> [Project]
projectsFromActions = nub . fmap projectFromAction
  where
    projectFromAction :: Action -> Project
    projectFromAction (Action _ _ p _ _) = p

contextsOfActions :: Actions -> Contexts
contextsOfActions a =  nub (a >>= contextsOfAnAction)
  where
    contextsOfAnAction :: Action -> Contexts
    contextsOfAnAction (Action _ _ _ c _) = c

modifyState :: Text -> Action -> ActionState -> Action
modifyState aid (Action actionId description project contexts state) newState
  | aid == actionId && state == ToDo = Action actionId
                                              description
                                              project
                                              contexts
                                              newState
modifyState _ action _ = action

