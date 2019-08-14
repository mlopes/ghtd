module CommandDispatcher
  ( dispatchCommand
  )
where

import qualified Data.UUID.V4                  as UUID4

import           Domain.Action
import           Domain.Command
import           Infra.YamlFileIO
import           Infra.Printer

dispatchCommand :: Command -> YamlFilePath -> IO ()
dispatchCommand Default filePath = listActions filePath
dispatchCommand (Add description project contexts) filePath =
  addNewAction description project contexts filePath
dispatchCommand (Complete actionId) filePath =
  changeActionState completeAction actionId filePath
dispatchCommand (Cancel actionId) filePath =
  changeActionState cancelAction actionId filePath

listActions :: Text -> IO ()
listActions filePath = readActions filePath >>= ghtdPrint

addNewAction :: Description -> Project -> Contexts -> YamlFilePath -> IO ()
addNewAction description project contexts filePath = do
  actions  <- readActions filePath
  actionId <- UUID4.nextRandom
  writeActionsAndOutputAction filePath $ addAction actions actionId description project contexts

writeActionsAndOutputAction :: YamlFilePath -> (Action, [Action]) -> IO ()
writeActionsAndOutputAction f (a, as) = writeActions f as >> ghtdPrint a

changeActionState
  :: ActionsModifier -> ActionId -> YamlFilePath -> IO ()
changeActionState actionsModifier aId filePath = do
  actions <- readActions filePath
  let newActions = actionsModifier actions aId
  _ <- writeActions filePath newActions
  ghtdPrint newActions

