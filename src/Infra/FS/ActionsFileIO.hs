{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Infra.FS.ActionsFileIO
  ( readActions
  , writeActions
  )
where

import qualified Data.UUID.V4                  as UUID4
import qualified Data.Yaml                     as Y
import           Data.String.Interpolate        ( i )
import           Domain.Action
import           Data.Yaml                      ( FromJSON(..)
                                                , (.:)
                                                , ToJSON(..)
                                                , (.=)
                                                )

import           Infra.FS.YamlFileIO

readActions :: YamlFilePath -> IO [Action]
readActions filePath = do
  actionId <- UUID4.nextRandom
  let (newAction, _) = addAction [] actionId "Welcome to ghtd. Run ghtd --help to know more." defaultProject defaultContexts
  readYamlFile [newAction] filePath

writeActions :: YamlFilePath -> [Action] -> IO ()
writeActions filePath = Y.encodeFile (unpack filePath)

instance FromJSON Action where
  parseJSON (Y.Object v) =
    Action
      <$> v
      .:  "action"
      <*> v
      .:  "description"
      <*> v
      .:  "project"
      <*> v
      .:  "contexts"
      <*> v
      .:  "state"
  parseJSON _ =
    fail "Expected YAML Object for an Action value, got something else instead."

instance FromJSON ActionState where
  parseJSON (Y.String s)
    | s == "To Do" = return ToDo
    | s == "Done" = return Done
    | s == "Cancelled" = return Cancelled
    | otherwise = fail
      [i|Expected one of 'To Do', 'Done', or 'Cancelled'. Found '#{s}'.|]
  parseJSON _ = fail "Expected 'state' to be a string, but got something else."

instance ToJSON Action where
  toJSON (Action actionId description project contexts state) = Y.object
    [ "action" .= actionId
    , "description" .= description
    , "project" .= project
    , "contexts" .= contexts
    , "state" .= state
    ]

instance ToJSON ActionState where
  toJSON ToDo      = Y.String "To Do"
  toJSON Done      = Y.String "Done"
  toJSON Cancelled = Y.String "Cancelled"

