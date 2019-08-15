{-# LANGUAGE ScopedTypeVariables #-}

module Infra.FileIO
  ( readYamlFile
  )
where


import           System.Directory
import qualified Data.Yaml                     as Y
import           Data.Yaml                      ( FromJSON(..)
                                                , ToJSON(..)
                                                )


type YamlFilePath = Text

readYamlFile :: (ToJSON a, FromJSON a) => YamlFilePath -> a -> IO a
readYamlFile filePath defaultOnNewfIle = do
  isFileThere <- fileExists
  if isFileThere
    then readFromFile filePath
    else writeToFile filePath defaultOnNewfIle >> readFromFile filePath
 where
  fileExists :: IO Bool
  fileExists = doesFileExist (unpack filePath)

readFromFile :: forall a . FromJSON a => YamlFilePath -> IO a
readFromFile f = do
  let filePath = unpack f
  fileReadResult <-
    Y.decodeFileEither filePath :: IO (Either Y.ParseException a)
  case fileReadResult of
    Left  e            -> fail $ Y.prettyPrintParseException e
    Right fileContents -> return fileContents

writeToFile :: ToJSON a => YamlFilePath -> a -> IO ()
writeToFile filePath = Y.encodeFile (unpack filePath)

