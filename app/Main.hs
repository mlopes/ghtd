{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where


import           ClassyPrelude

import           CliCommand
import           CommandDispatcher

main :: IO ()
main = do
  command <- resolveCliCommand
  dispatchCommand command yamlFilePath

yamlFilePath :: Text
yamlFilePath = "./data/sample.yaml"
