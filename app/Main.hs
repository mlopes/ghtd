{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Infra.CliParser
import           CommandDispatcher

main :: IO ()
main = do
  command <- resolveCommand
  dispatchCommand command yamlFilePath

yamlFilePath :: Text
yamlFilePath = "./data/sample.yaml"
