module Main where

import Command.CommandResolver
import qualified Data.Foldable as F
import Lib
import qualified Options.Applicative as O

main :: IO ()
main = do
  command <- O.execParser parserInfo
  case command of
    Nothing -> showActionsFromYaml yamlFilePath
    Just x -> print x
  return ()

yamlFilePath :: String
yamlFilePath = "./data/sample.yaml"
