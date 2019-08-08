module Main where

import Command.CommandDispatcher
import Command.CommandResolver

main :: IO ()
main = do
  command <- resolveCommand
  dispatchCommand command yamlFilePath

yamlFilePath :: String
yamlFilePath = "./data/sample.yaml"
