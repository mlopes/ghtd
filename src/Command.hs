{-# LANGUAGE OverloadedStrings #-}

module Command
  ( resolveCommand
  , Command(..)
  )
where

import qualified Data.Foldable                 as F
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as L
import qualified Options.Applicative           as O

data Command
  = Add Text Text [Text]
  | Remove
  | Projects
  | Contexts
  | Help
  deriving (Show)

resolveCommand :: IO (Maybe Command)
resolveCommand = O.execParser parserInfo

parserInfo :: O.ParserInfo (Maybe Command)
parserInfo = info' parser' "This is the main prog desc"

parser' :: O.Parser (Maybe Command)
parser' = O.optional $ (O.subparser . F.foldMap command)
  [ ( "add"
    , "Add new action"
    , Add <$> actionParam <*> projectOption <*> contextOptions
    )
  , ("rm"      , "Remove an action"         , pure Remove)
  , ("projects", "Show projects"            , pure Projects)
  , ("contexts", "Show contexts"            , pure Contexts)
  , ("help"    , "Help I'm being repressed!", pure Help)
  ]

info' :: O.Parser a -> String -> O.ParserInfo a
info' p desc = O.info (O.helper <*> p) (O.fullDesc <> O.progDesc desc)

command (cmdName, desc, parser) = O.command cmdName (info' parser desc)

actionParam :: O.Parser Text
actionParam = O.argument O.str (O.metavar "ACTION")

-- Saving this here as a reference on how to parse a bunch of optional positional arguments
-- addParams :: O.Parser (Maybe [String])
-- addParams = O.optional $ O.some (O.argument O.str (O.metavar "FILTER..."))
projectOption = O.strOption
  (mconcat
    [ O.help "Project name."
    , O.value "inbox"
    , O.showDefault
    , O.long "project"
    , O.short 'p'
    , O.metavar "PROJECT"
    ]
  )

contextOptions = fmap (splitStringToTextOn ",") contextAsStringParser

contextAsStringParser = O.strOption
  (mconcat
    [ O.help "Contexts for the action."
    , O.value ""
    , O.showDefault
    , O.long "contexts"
    , O.short 'c'
    , O.metavar "CONTEXTS"
    ]
  )

splitStringToTextOn :: Text -> String -> [Text]
splitStringToTextOn s = L.splitOn s . L.pack

