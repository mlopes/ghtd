module Action
  ( Action(..)
  , ActionState(..)
  , ActionId
  , Description
  , Project
  , Contexts
  , addAction
  , completeAction
  , cancelAction
  )
where

import Data.UUID

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

addAction :: [Action] -> UUID -> Description -> Project -> Contexts -> [Action]
addAction as a d p c = Action (textNoDashesUUID4 a) d p c ToDo : as

completeAction :: [Action] -> ActionId -> [Action]
completeAction actions aId = fmap (\x -> modifyState aId x Done) actions

cancelAction :: [Action] -> ActionId -> [Action]
cancelAction actions aId = fmap (\x -> modifyState aId x Cancelled) actions

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

