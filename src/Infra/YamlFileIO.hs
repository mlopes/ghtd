{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Infra.YamlFileIO
  ( readActions
  , addAction
  )
where

import           Control.Applicative
import           Control.Monad
import           Data.Bool
import           Data.Either
import           Data.Eq
import           Data.Function
import           Data.Text.Lazy                 ( Text
                                                , unpack
                                                )
import qualified Data.Yaml                     as Y
import           Data.String.Interpolate        ( i )
import           Action
import           Data.Yaml                      ( FromJSON(..)
                                                , (.:)
                                                , ToJSON(..)
                                                , (.=)
                                                )
import           System.IO

type YamlFilePath = Text

addAction :: YamlFilePath -> Action -> IO ()
addAction f a = do
  let filePath = unpack f
  existingActions <- readActions f
  let newActionList = a : existingActions
  Y.encodeFile filePath newActionList

readActions :: YamlFilePath -> IO [Action]
readActions f = do
  let filePath = unpack f
  fileReadResult <-
    Y.decodeFileEither filePath :: IO (Either Y.ParseException [Action])
  case fileReadResult of
    Left  e  -> fail $ Y.prettyPrintParseException e
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

