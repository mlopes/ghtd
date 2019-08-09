module Main where

import Command
import CommandDispatcher

main :: IO ()
main = do
  command <- resolveCommand
  dispatchCommand command yamlFilePath

yamlFilePath :: String
yamlFilePath = "./data/sample.yaml"
