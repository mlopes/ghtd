{-# LANGUAGE OverloadedStrings #-}

module Domain.Action
  ( Action(..)
  , ActionState(..)
  , ActionId
  , Description
  , Project
  , Contexts
  , ActionsModifier
  , defaultProject
  , defaultContexts
  , addAction
  , completeAction
  , cancelAction
  , projectsFromActions
  )
where

import Data.UUID
import Data.List (nub)

type ActionId = Text
type Description = Text
type Project = Text
type Contexts = [Text]

type ActionsModifier = [Action] -> ActionId -> [Action]

data ActionState = ToDo
                 | Done
                 | Cancelled
                 deriving Eq

data Action =
  Action ActionId Description Project Contexts ActionState

defaultProject :: Text
defaultProject = "Inbox"

defaultContexts :: [Text]
defaultContexts = []

addAction :: [Action] -> UUID -> Description -> Project -> Contexts -> (Action, [Action])
addAction as a d p c =
  (action, action : as)
  where action = Action (textNoDashesUUID4 a) d p c ToDo

completeAction :: ActionsModifier
completeAction actions aId = fmap (\x -> modifyState aId x Done) actions

cancelAction :: ActionsModifier
cancelAction actions aId = fmap (\x -> modifyState aId x Cancelled) actions

projectsFromActions :: [Action] -> [Project]
projectsFromActions = nub . fmap projectFromAction

projectFromAction :: Action -> Project
projectFromAction (Action _ _ p _ _) = p

modifyState :: Text -> Action -> ActionState -> Action
modifyState aid (Action actionId description project contexts state) newState
  | aid == actionId && state == ToDo = Action actionId
                                              description
                                              project
                                              contexts
                                              newState
modifyState _ action _ = action

textNoDashesUUID4 :: UUID -> Text
textNoDashesUUID4 = stripDashes . toText

stripDashes :: Text -> Text
stripDashes = filter ('-' /=)

