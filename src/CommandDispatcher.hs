module CommandDispatcher
  ( dispatchCommand
  )
where

import qualified Data.UUID.V4                  as UUID4

import           Domain.Action
import           Domain.Action.Types
import           Domain.Command
import           Domain.Scope
import           Infra.FS.ActionsFileIO
import           Infra.FS.YamlFileIO            ( YamlFilePath )
import           Infra.Printer

dispatchCommand :: Scope -> Command -> YamlFilePath -> IO ()
dispatchCommand scope Default filePath = listActions (scopedView scope)
 where
  listActions :: (Actions -> ScopeView) -> IO ()
  listActions scopeViewer = scopeViewer <$> getActions >>= ghtdPrint
  getActions :: IO Actions
  getActions = readActions filePath

dispatchCommand _ (Add description project contexts) filePath = addNewAction
 where
  addNewAction = do
    actions  <- readActions filePath
    actionId <- UUID4.nextRandom
    let action = createActionWithUUID actionId
                                      description
                                      (fromMaybe defaultProject project)
                                      (fromMaybe defaultContexts contexts)
    writeActionsAndOutputAction (action, addAction action actions)
  writeActionsAndOutputAction :: (Action, Actions) -> IO ()
  writeActionsAndOutputAction (a, as) = writeActions filePath as >> ghtdPrint a

dispatchCommand _ (Complete actionId) filePath =
  changeActionState completeAction actionId filePath

dispatchCommand _ (Cancel actionId) filePath =
  changeActionState cancelAction actionId filePath

dispatchCommand _ ListProjects filePath = listProjects
  where listProjects = readActions filePath >>= ghtdPrint . projectsFromActions

dispatchCommand _ ListContexts filePath = listContexts
  where listContexts = readActions filePath >>= ghtdPrint . contextsOfActions

changeActionState :: ActionsModifier -> ActionId -> YamlFilePath -> IO ()
changeActionState actionsModifier aId filePath = do
  actions <- readActions filePath
  let newActions = actionsModifier actions aId
  _ <- writeActions filePath newActions
  ghtdPrint newActions

