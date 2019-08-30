{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Infra.FS.ScopeFileIO
import           Infra.CliParser
import           CommandDispatcher

main :: IO ()
main = do
  scope <- loadScope scopeFilePath
  command <- resolveCommand
  dispatchCommand scope command actionsFilePath

actionsFilePath :: Text
actionsFilePath = "./data/sample.yaml"

scopeFilePath :: Text
scopeFilePath = "./data/scope.yaml"

