{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Infra.CliParser
import           CommandDispatcher

main :: IO ()
main = do
  command <- resolveCommand
  dispatchCommand command actionsFilePath

actionsFilePath :: Text
actionsFilePath = "./data/sample.yaml"

checkoutFilePath :: Text
checkoutFilePath = "./data/checkout.yaml"
