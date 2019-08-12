{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text.Lazy                 ( Text )
import           System.IO

import           CliCommand
import           CommandDispatcher

main :: IO ()
main = do
  command <- resolveCliCommand
  dispatchCommand command yamlFilePath

yamlFilePath :: Text
yamlFilePath = "./data/sample.yaml"
