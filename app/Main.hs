module Main where

import Command

main :: IO ()
main = do
  command <- resolveCommand
  dispatchCommand command yamlFilePath

yamlFilePath :: String
yamlFilePath = "./data/sample.yaml"
