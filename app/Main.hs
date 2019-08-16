{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Infra.FS.ScopeFileIO
import           Infra.CliParser
import           CommandDispatcher

main :: IO ()
main = do
  _ <- loadScope scopeFilePath
  command <- resolveCommand
  dispatchCommand command actionsFilePath

actionsFilePath :: Text
actionsFilePath = "./data/sample.yaml"

scopeFilePath :: Text
scopeFilePath = "./data/scope.yaml"

