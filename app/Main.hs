module Main where

import Lib
import qualified Options.Applicative as O
import qualified Data.Foldable as F

data Command =
    Add
  | Remove
  | Projects
  | Contexts
  | Help
  deriving Show

parserInfo' :: O.ParserInfo (Maybe Command)
parserInfo' = info' parser' "This is the main prog desc"
  where
    parser' :: O.Parser (Maybe Command)
    parser' = O.optional $ (O.subparser . F.foldMap command')
        [ ("add", "Add new action", pure Add)
        , ("rm", "Remove an action", pure Remove)
        , ("projects", "Show projects", pure Projects)
        , ("contexts", "Show contexts", pure Contexts)
        , ("help", "Help I'm being repressed!", pure Help)
        ]

    info' :: O.Parser a -> String -> O.ParserInfo a
    info' p desc = O.info
        (O.helper <*> p)
        (O.fullDesc <> O.progDesc desc)

    command' (cmdName,desc,parser) =
        O.command cmdName (info' parser desc)

main :: IO ()
main = do
  command <- O.execParser parserInfo'
  case command of
    Nothing -> showActionsFromYaml yamlFilePath
    Just x  -> print x
  return ()

yamlFilePath :: String
yamlFilePath = "./data/sample.yaml"
