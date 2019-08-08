module Main where

import Command.CommandDispatcher
import Command.CommandResolver
import qualified Data.Foldable as F
import Lib
import qualified Options.Applicative as O

main :: IO ()
main = do
  command <- O.execParser parserInfo
  dispatchCommand command yamlFilePath

yamlFilePath :: String
yamlFilePath = "./data/sample.yaml"
