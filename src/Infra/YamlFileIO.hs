{-# LANGUAGE OverloadedStrings #-}

module Infra.YamlFileIO
  ( readActions
  , addAction
  )
where

import qualified Data.Yaml                     as Y
import           Data.Text.Lazy                 ( Text )
import           Action
import           Data.Yaml                      ( FromJSON(..)
                                                , (.:)
                                                , ToJSON(..)
                                                , (.=)
                                                )

data FileFailure e = ReadFailure e | WriteFailure e

type YamlFilePath = String

data ActionYaml =
  ActionYaml
    { action :: Text
    , description :: Text
    , project :: Text
    , contexts :: [Text]
    }
  deriving (Show)

readActions :: YamlFilePath -> IO [Action]
readActions file = do
  yaml <- yamlFromFile file
  return $ fromYamlToDomain <$> yaml

addAction :: YamlFilePath -> Action -> IO ()
addAction f a = do
  existingActions <- yamlFromFile f
  let newActionList = fromDomainToYaml a : existingActions
  Y.encodeFile f newActionList

yamlFromFile :: YamlFilePath -> IO [ActionYaml]
yamlFromFile file = do
  fileReadResult <-
    Y.decodeFileEither file :: IO (Either Y.ParseException [ActionYaml])
  case fileReadResult of
    Left  e  -> error $ Y.prettyPrintParseException e
    Right as -> return as

fromDomainToYaml :: Action -> ActionYaml
fromDomainToYaml (Action actionId description project contexts status) =
  ActionYaml { action      = actionId
             , description = description
             , project     = project
             , contexts    = contexts
             }

fromYamlToDomain :: ActionYaml -> Action
fromYamlToDomain y =
  Action (action y) (description y) (project y) (contexts y) ToDo -- ToDo hardcoded while we don't deal with statuses

instance FromJSON ActionYaml where
  parseJSON (Y.Object v) =
    ActionYaml
      <$> v
      .:  "action"
      <*> v
      .:  "description"
      <*> v
      .:  "project"
      <*> v
      .:  "contexts"
  parseJSON _ = fail "Expected Object for Action value"

instance ToJSON ActionYaml where
  toJSON a = Y.object
    [ "action" .= action a
    , "description" .= description a
    , "project" .= project a
    , "contexts" .= contexts a
    ]
