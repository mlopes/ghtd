module Command.CommandDispatcher
  ( dispatchCommand
  ) where

import Command.Command
import Lib

dispatchCommand :: Maybe Command -> String -> IO ()
dispatchCommand Nothing yamlFilePath = showActionsFromYaml yamlFilePath
dispatchCommand (Just x) _ = print x
