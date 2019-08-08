module Command
  ( Command(..)
  , dispatchCommand
  , resolveCommand
  ) where

import qualified Data.Foldable as F
import Lib
import qualified Options.Applicative as O

data Command
  = Add
  | Remove
  | Projects
  | Contexts
  | Help
  deriving (Show)

dispatchCommand :: Maybe Command -> String -> IO ()
dispatchCommand Nothing yamlFilePath = showActionsFromYaml yamlFilePath
dispatchCommand (Just x) _ = print x

resolveCommand :: IO (Maybe Command)
resolveCommand = O.execParser parserInfo

parserInfo :: O.ParserInfo (Maybe Command)
parserInfo = info' parser' "This is the main prog desc"
  where
    parser' :: O.Parser (Maybe Command)
    parser' =
      O.optional $
      (O.subparser . F.foldMap command')
        [ ("add", "Add new action", pure Add)
        , ("rm", "Remove an action", pure Remove)
        , ("projects", "Show projects", pure Projects)
        , ("contexts", "Show contexts", pure Contexts)
        , ("help", "Help I'm being repressed!", pure Help)
        ]
    info' :: O.Parser a -> String -> O.ParserInfo a
    info' p desc = O.info (O.helper <*> p) (O.fullDesc <> O.progDesc desc)
    command' (cmdName, desc, parser) = O.command cmdName (info' parser desc)
