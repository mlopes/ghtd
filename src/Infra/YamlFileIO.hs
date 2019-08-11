{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Infra.YamlFileIO
  ( readActions
  , addAction
  )
where

import qualified Data.Yaml                     as Y
import           Data.String.Interpolate        ( i )
import           Data.Text.Lazy                 ( Text )
import           Action
import           Data.Yaml                      ( FromJSON(..)
                                                , (.:)
                                                , ToJSON(..)
                                                , (.=)
                                                )

data FileFailure e = ReadFailure e | WriteFailure e

type YamlFilePath = String

addAction :: YamlFilePath -> Action -> IO ()
addAction f a = do
  existingActions <- readActions f
  let newActionList = a : existingActions
  Y.encodeFile f newActionList

readActions :: YamlFilePath -> IO [Action]
readActions file = do
  fileReadResult <-
    Y.decodeFileEither file :: IO (Either Y.ParseException [Action])
  case fileReadResult of
    Left  e  -> error $ Y.prettyPrintParseException e
    Right as -> return as

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
      .: "state"
  parseJSON _ = fail "Expected Object for Action value"

instance FromJSON ActionState where
    parseJSON (Y.String s)
      | s == "To Do" = return ToDo
      | s == "Done" = return Done
      | s == "Cancelled" = return Cancelled
      | otherwise = fail [i|Expected one of 'To Do', 'Done', or 'Cancelled'. Found '#{s}'.|]

instance ToJSON Action where
  toJSON (Action actionId description project contexts state) = Y.object
    [ "action" .= actionId
    , "description" .= description
    , "project" .= project
    , "contexts" .= contexts
    , "state" .= state
    ]

instance ToJSON ActionState where
  toJSON ToDo = Y.String "To Do"
  toJSON Done = Y.String "Done"
  toJSON Cancelled = Y.String "Cancelled"

