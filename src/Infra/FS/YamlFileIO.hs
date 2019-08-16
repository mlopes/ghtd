{-# LANGUAGE ScopedTypeVariables #-}

module Infra.FS.YamlFileIO
  ( readYamlFile
  , writeYamlFile
  , YamlFilePath
  )
where


import           System.Directory
import qualified Data.Yaml                     as Y
import           Data.Yaml                      ( FromJSON(..)
                                                , ToJSON(..)
                                                )


type YamlFilePath = Text

readYamlFile :: (ToJSON a, FromJSON a) => a -> YamlFilePath -> IO a
readYamlFile defaultOnNewfIle filePath = do
  isFileThere <- fileExists
  if isFileThere
    then readFromFile filePath
    else writeYamlFile filePath defaultOnNewfIle >> readFromFile filePath
 where
  fileExists :: IO Bool
  fileExists = doesFileExist (unpack filePath)

writeYamlFile :: ToJSON a => YamlFilePath -> a -> IO ()
writeYamlFile filePath = Y.encodeFile (unpack filePath)

readFromFile :: forall a . FromJSON a => YamlFilePath -> IO a
readFromFile f = do
  let filePath = unpack f
  fileReadResult <-
    Y.decodeFileEither filePath :: IO (Either Y.ParseException a)
  case fileReadResult of
    Left  e            -> fail $ Y.prettyPrintParseException e
    Right fileContents -> return fileContents

