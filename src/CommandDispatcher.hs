module CommandDispatcher
  ( dispatchCommand
  )
where

import qualified Data.UUID.V4                  as UUID4

import           Action
import           CliCommand
import           Infra.YamlFileIO
import           Formatter

dispatchCommand :: Maybe CliCommand -> Text -> IO ()
dispatchCommand Nothing filePath = do
  actions <- readActions filePath
  putStrLn (format actions)
dispatchCommand (Just (Add description project contexts)) filePath = do
  actionId <- UUID4.nextRandom
  actions  <- readActions filePath
  let newActions = addAction actions actionId description project contexts
  _ <- writeActions filePath newActions
  putStrLn $ format newActions
dispatchCommand (Just (Complete actionId)) filePath =
  changeActionState completeAction actionId filePath
dispatchCommand (Just (Cancel actionId)) filePath =
  changeActionState cancelAction actionId filePath
dispatchCommand (Just x) _ = print x

changeActionState
  :: ([Action] -> ActionId -> [Action]) -> ActionId -> Text -> IO ()
changeActionState f aId filePath = do
  actions <- readActions filePath
  let newActions = f actions aId
  _ <- writeActions filePath newActions
  putStrLn $ format newActions

