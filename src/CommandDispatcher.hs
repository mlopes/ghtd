module CommandDispatcher
  ( dispatchCommand
  )
where

import qualified Data.UUID.V4                  as UUID4

import           Domain.Action
import           Domain.Command
import           Infra.ActionsFileIO
import           Infra.YamlFileIO
import           Infra.Printer

dispatchCommand :: Command -> YamlFilePath -> IO ()
dispatchCommand Default filePath = listActions
  where
    listActions ::  IO ()
    listActions = readActions filePath >>= ghtdPrint
dispatchCommand (Add description project contexts) filePath = addNewAction
  where
    addNewAction = do
      actions  <- readActions filePath
      actionId <- UUID4.nextRandom
      writeActionsAndOutputAction $ addAction actions actionId description project contexts
    writeActionsAndOutputAction :: (Action, [Action]) -> IO ()
    writeActionsAndOutputAction (a, as) = writeActions filePath as >> ghtdPrint a
dispatchCommand (Complete actionId) filePath =
  changeActionState completeAction actionId filePath
dispatchCommand (Cancel actionId) filePath =
  changeActionState cancelAction actionId filePath
dispatchCommand ListProjects filePath = listProjects
  where
    listProjects = readActions filePath >>= ghtdPrint . projectsFromActions
dispatchCommand ListContexts filePath = listContexts
  where
    listContexts = readActions filePath >>= ghtdPrint . contextsFromActions

changeActionState
  :: ActionsModifier -> ActionId -> YamlFilePath -> IO ()
changeActionState actionsModifier aId filePath = do
  actions <- readActions filePath
  let newActions = actionsModifier actions aId
  _ <- writeActions filePath newActions
  ghtdPrint newActions

