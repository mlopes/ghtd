module Main where

import           CliCommand
import           CommandDispatcher

main :: IO ()
main = do
  command <- resolveCliCommand
  dispatchCommand command yamlFilePath

yamlFilePath :: String
yamlFilePath = "./data/sample.yaml"
