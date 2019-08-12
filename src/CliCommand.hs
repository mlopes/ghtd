{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CliCommand
  ( resolveCliCommand
  , CliCommand(..)
  )
where

import           ClassyPrelude
import           Data.Text                      ( splitOn )
import qualified Data.Foldable                 as F
import qualified Options.Applicative           as O

data CliCommand
  = Add Description Project Contexts
  | Complete ActionId
  | Cancel ActionId
  | Remove
  | Projects
  | Contexts
  | Help
  deriving (Show)

resolveCliCommand :: IO (Maybe CliCommand)
resolveCliCommand = O.execParser parserInfo

type ActionId = Text
type Description = Text
type Project = Text
type Contexts = [Text]

parserInfo :: O.ParserInfo (Maybe CliCommand)
parserInfo = info' parser' "This is the main prog desc"

parser' :: O.Parser (Maybe CliCommand)
parser' = O.optional $ (O.subparser . F.foldMap command)
  [ ( "add"
    , "Add new action"
    , Add <$> actionParam <*> projectOption <*> contextOptions
    )
  , ("complete", "Complete an action."      , Complete <$> actionIdParam)
  , ("cancel"  , "Cancel an action."        , Cancel <$> actionIdParam)
  , ("rm"      , "Remove an action"         , pure Remove)
  , ("projects", "Show projects"            , pure Projects)
  , ("contexts", "Show contexts"            , pure Contexts)
  , ("help"    , "Help I'm being repressed!", pure Help)
  ]

info' :: O.Parser a -> Text -> O.ParserInfo a
info' p desc = O.info (O.helper <*> p) (O.fullDesc <> O.progDesc (unpack desc))

command :: (Text, Text, O.Parser a) -> O.Mod O.CommandFields a
command (cmdName, desc, parser) =
  O.command (unpack cmdName) (info' parser desc)

actionParam :: O.Parser Text
actionParam = O.argument O.str (O.metavar "ACTION")

actionIdParam :: O.Parser Text
actionIdParam = O.argument O.str (O.metavar "ACTION_ID")

-- Saving this here as a reference on how to parse a bunch of optional positional arguments
-- addParams :: O.Parser (Maybe [String])
-- addParams = O.optional $ O.some (O.argument O.str (O.metavar "FILTER..."))
projectOption :: O.Parser Project
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

contextOptions :: O.Parser Contexts
contextOptions = fmap (splitOn ",") contextAsStringParser

contextAsStringParser :: O.Parser Text
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

