module Domain.Private.Action.Action
  ( createActionWithUUID
  , addAction
  , completeAction
  , cancelAction
  )
where

import Data.UUID

import Domain.Action.Types

createActionWithUUID :: UUID -> Description -> Project -> Contexts -> Action
createActionWithUUID uuid description project contexts = Action (textNoDashesUUID4 uuid) description project contexts ToDo
  where
    textNoDashesUUID4 :: UUID -> Text
    textNoDashesUUID4 = stripDashes . toText
    stripDashes :: Text -> Text
    stripDashes = filter ('-' /=)

addAction :: Action -> Actions -> Actions
addAction action originalActions = action : originalActions

completeAction :: ActionsModifier
completeAction actions aId = fmap (\x -> modifyState aId x Done) actions

cancelAction :: ActionsModifier
cancelAction actions aId = fmap (\x -> modifyState aId x Cancelled) actions

modifyState :: Text -> Action -> ActionState -> Action
modifyState aid (Action actionId description project contexts state) newState
  | aid == actionId && state == ToDo = Action actionId
                                              description
                                              project
                                              contexts
                                              newState
modifyState _ action _ = action

