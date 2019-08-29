module CommandDispatcher
  ( dispatchCommand
  )
where

import qualified Data.UUID.V4                  as UUID4

import           Domain.Action
import Domain.Contexts
import           Domain.Command
import           Infra.FS.ActionsFileIO
import           Infra.FS.YamlFileIO            ( YamlFilePath )
import           Infra.Printer

dispatchCommand :: Command -> YamlFilePath -> IO ()
dispatchCommand Default filePath = listActions
  where
    listActions :: IO ()
    listActions = readActions filePath >>= ghtdPrint

dispatchCommand (Add description project contexts) filePath = addNewAction
  where
    extractA :: Maybe a -> a -> a
    extractA (Just a) _ = a
    extractA Nothing f = f
    addNewAction = do
      actions  <- readActions filePath
      actionId <- UUID4.nextRandom
      writeActionsAndOutputAction
        $ addAction actions actionId description (extractA project defaultProject) (extractA contexts defaultContexts)
    writeActionsAndOutputAction :: (Action, [Action]) -> IO ()
    writeActionsAndOutputAction (a, as) = writeActions filePath as >> ghtdPrint a

dispatchCommand (Complete actionId) filePath =
  changeActionState completeAction actionId filePath

dispatchCommand (Cancel actionId) filePath =
  changeActionState cancelAction actionId filePath

dispatchCommand ListProjects filePath = listProjects
  where listProjects = readActions filePath >>= ghtdPrint . projectsFromActions

dispatchCommand ListContexts filePath = listContexts
  where listContexts = readActions filePath >>= ghtdPrint . contextsOfActions

changeActionState :: ActionsModifier -> ActionId -> YamlFilePath -> IO ()
changeActionState actionsModifier aId filePath = do
  actions <- readActions filePath
  let newActions = actionsModifier actions aId
  _ <- writeActions filePath newActions
  ghtdPrint newActions

