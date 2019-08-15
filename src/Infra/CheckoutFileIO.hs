{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Infra.CheckoutFileIO
  ( readCheckoutState
  )
where


import           System.Directory
import           Data.String.Interpolate        ( i )
import qualified Data.Yaml                     as Y
import           Data.Yaml                      ( FromJSON(..)
                                                , (.:)
                                                , ToJSON(..)
                                                , (.=)
                                                )


import           Domain.Action

type CheckoutFilePath = Text

data CheckoutState = CheckoutState CheckoutObject CheckoutObjectName

data CheckoutObject = ProjectObject | ContextObject

type CheckoutObjectName = Text

readCheckoutState :: CheckoutFilePath -> IO CheckoutState
readCheckoutState filePath = do
  isFileThere <- fileExists
  if isFileThere then readCheckoutStateFile filePath else
    writeCheckoutStateFile filePath (CheckoutState ProjectObject defaultProject)
      >> readCheckoutStateFile filePath
 where
  fileExists :: IO Bool
  fileExists = doesFileExist (unpack filePath)

readCheckoutStateFile :: CheckoutFilePath -> IO CheckoutState
readCheckoutStateFile f = do
  let filePath = unpack f
  fileReadResult <-
    Y.decodeFileEither filePath :: IO (Either Y.ParseException CheckoutState)
  case fileReadResult of
    Left  e  -> fail $ Y.prettyPrintParseException e
    Right cs -> return cs

writeCheckoutStateFile :: CheckoutFilePath -> CheckoutState -> IO ()
writeCheckoutStateFile filePath = Y.encodeFile (unpack filePath)


instance FromJSON CheckoutState where
  parseJSON (Y.Object v) = CheckoutState <$> v .: "object" <*> v .: "name"
  parseJSON _ =
    fail "Expected YAML Object for an Action value, got something else instead."

instance FromJSON CheckoutObject where
  parseJSON (Y.String s)
    | s == "Project"
    = return ProjectObject
    | s == "Context"
    = return ContextObject
    | otherwise
    = fail [i|Expected one of 'Project', or 'Context. Found '#{s}'.|]
  parseJSON _ =
    fail "Expected checkout objet to be a string, but got something else."

instance ToJSON CheckoutState where
  toJSON (CheckoutState checkoutObject checkoutObjectName) =
    Y.object ["object" .= checkoutObject, "name" .= checkoutObjectName]

instance ToJSON CheckoutObject where
  toJSON ProjectObject = Y.String "Project"
  toJSON ContextObject = Y.String "Context"

