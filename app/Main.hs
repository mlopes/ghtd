{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Command
import           CommandDispatcher

main :: IO ()
main = do
  command <- resolveCommand
  dispatchCommand command yamlFilePath

yamlFilePath :: Text
yamlFilePath = "./data/sample.yaml"
