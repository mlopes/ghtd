module Lib
  ( showActionsFromYaml
  ) where

import qualified Data.Text.Lazy.IO as I (putStrLn)
import qualified Data.Yaml as Y
import Formatter
import Model

showActionsFromYaml :: String -> IO ()
showActionsFromYaml f = do
  x <- yamlFromFile f
  case x of
    Left y -> putStr $ Y.prettyPrintParseException y -- "Error parsing the file"
    Right y -> I.putStrLn $ stringifyActions y

yamlFromFile :: String -> IO (Either Y.ParseException [Actions])
yamlFromFile file =
  Y.decodeFileEither file :: IO (Either Y.ParseException [Actions])
