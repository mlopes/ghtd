module CommandDispatcher
  ( dispatchCommand
  )
where

import qualified Data.UUID.V4                  as UUID4

import           Action
import           Command
import           Infra.YamlFileIO
import           Formatter

dispatchCommand :: Command -> YamlFilePath -> IO ()
dispatchCommand Default filePath = listActions filePath
dispatchCommand (Add description project contexts) filePath =
  addNewAction description project contexts filePath
dispatchCommand (Complete actionId) filePath =
  changeActionState completeAction actionId filePath
dispatchCommand (Cancel actionId) filePath =
  changeActionState cancelAction actionId filePath

listActions :: Text -> IO ()
listActions filePath = do
  actions <- readActions filePath
  putStrLn (format actions)

addNewAction :: Description -> Project -> Contexts -> YamlFilePath -> IO ()
addNewAction description project contexts filePath = do
  actions  <- readActions filePath
  actionId <- UUID4.nextRandom
  let newActions = addAction actions actionId description project contexts
  _ <- writeActions filePath newActions
  putStrLn $ format newActions

changeActionState
  :: ActionsModifier -> ActionId -> YamlFilePath -> IO ()
changeActionState actionsModifier aId filePath = do
  actions <- readActions filePath
  let newActions = actionsModifier actions aId
  _ <- writeActions filePath newActions
  putStrLn $ format newActions

