{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Infra.FS.ScopeFileIO
  ( loadScope
  )
where

import           Data.String.Interpolate        ( i )
import qualified Data.Yaml                     as Y
import           Data.Yaml                      ( FromJSON(..)
                                                , (.:)
                                                , ToJSON(..)
                                                , (.=)
                                                )

import           Infra.FS.YamlFileIO
import           Domain.Action
import           Domain.Scope

loadScope :: YamlFilePath -> IO Scope
loadScope = readYamlFile (Scope ProjectScope defaultProject)

instance FromJSON Scope where
  parseJSON (Y.Object v) = Scope <$> v .: "type" <*> v .: "name"
  parseJSON _ =
    fail "Expected YAML Object for an Action value, got something else instead."

instance FromJSON ScopeType where
  parseJSON (Y.String s)
    | s == "Project"
    = return ProjectScope
    | s == "Context"
    = return ContextScope
    | otherwise
    = fail [i|Expected one of 'Project', or 'Context. Found '#{s}'.|]
  parseJSON _ =
    fail "Expected Scope type to be a string, but got something else."

instance ToJSON Scope where
  toJSON (Scope scopeType scopeName) =
    Y.object ["type" .= scopeType, "name" .= scopeName]

instance ToJSON ScopeType where
  toJSON ProjectScope = Y.String "Project"
  toJSON ContextScope = Y.String "Context"

