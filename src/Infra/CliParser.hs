{-# LANGUAGE OverloadedStrings #-}

module Infra.CliParser
  ( resolveCommand
  )
where

import           Data.Text                      ( splitOn )
import qualified Data.Foldable                 as F
import qualified Options.Applicative           as O

import Domain.Command
import Domain.Action

data CliCommand
  = CmdAdd Description Project Contexts
  | CmdComplete ActionId
  | CmdCancel ActionId

resolveCommand :: IO Command
resolveCommand = mapCliCommandToCommand <$> O.execParser parserInfo

mapCliCommandToCommand :: Maybe CliCommand -> Command
mapCliCommandToCommand Nothing                = Default
mapCliCommandToCommand (Just (CmdAdd d p c )) = Add d p c
mapCliCommandToCommand (Just (CmdComplete a)) = Complete a
mapCliCommandToCommand (Just (CmdCancel   a)) = Cancel a

parserInfo :: O.ParserInfo (Maybe CliCommand)
parserInfo = info' parser' "This is the main prog desc"

parser' :: O.Parser (Maybe CliCommand)
parser' = O.optional $ (O.subparser . F.foldMap command)
  [ ( "add"
    , "Add new action"
    , CmdAdd <$> actionParam <*> projectOption <*> contextOptions
    )
  , ("complete", "Complete an action.", CmdComplete <$> actionIdParam)
  , ("cancel"  , "Cancel an action."  , CmdCancel <$> actionIdParam)
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
    , O.value defaultProject
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
    , O.value $ intercalate "," defaultContexts
    , O.showDefault
    , O.long "contexts"
    , O.short 'c'
    , O.metavar "CONTEXTS"
    ]
  )

