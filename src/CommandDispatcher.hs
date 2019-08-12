{-# LANGUAGE NoImplicitPrelude #-}

module CommandDispatcher
  ( dispatchCommand
  )
where

import           ClassyPrelude
import           Data.UUID
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
  actionId <- textNoDashesUUID4
  let action = Action actionId description project contexts ToDo
  _ <- addAction filePath action
  putStrLn $ format action

dispatchCommand (Just (Complete actionId)) filePath =
  changeActionState Done actionId filePath
dispatchCommand (Just (Cancel actionId)) filePath =
  changeActionState Cancelled actionId filePath
dispatchCommand (Just x) _ = print x

textNoDashesUUID4 :: IO Text
textNoDashesUUID4 = stripDashes . toText <$> UUID4.nextRandom

stripDashes :: Text -> Text
stripDashes = filter ('-' /=)

changeActionState :: ActionState -> Text -> Text -> IO ()
changeActionState newState aId filePath = do
  actions <- readActions filePath
  let newActions = fmap (\x -> modifyState aId x newState) actions
  _ <- writeActions filePath newActions
  putStrLn $ format newActions

modifyState :: Text -> Action -> ActionState -> Action
modifyState aid (Action actionId description project contexts state) newState
  | aid == actionId && state == ToDo = Action actionId
                                              description
                                              project
                                              contexts
                                              newState
modifyState _ action _ = action

