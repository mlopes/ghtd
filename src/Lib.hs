module Lib
  ( showActions
  , addAction
  )
where

import qualified Data.Text.Lazy.IO             as I
                                                ( putStrLn )
import qualified Data.Yaml                     as Y
import           Data.Functor
import           Formatter
import           Model

type YamlFilePath = String

showActions :: YamlFilePath -> IO ()
showActions f = do
  x <- yamlFromFile f
  case x of
    Left  y -> putStr $ Y.prettyPrintParseException y -- "Error parsing the file"
    Right y -> I.putStrLn $ stringifyActions y

addAction :: YamlFilePath -> Actions -> IO ()
addAction f a = do
  y <- yamlFromFile f
  case y of
    Left  e  -> putStr $ Y.prettyPrintParseException e -- "Error parsing the file"
    Right as -> writeActionsToFile f (a : as) >> I.putStrLn (actionToText a)

writeActionsToFile :: YamlFilePath -> [Actions] -> IO ()
writeActionsToFile = Y.encodeFile

yamlFromFile :: String -> IO (Either Y.ParseException [Actions])
yamlFromFile file =
  Y.decodeFileEither file :: IO (Either Y.ParseException [Actions])

