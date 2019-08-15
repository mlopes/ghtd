{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Infra.CheckoutFileIO
  ( readCheckoutState
  )
where


import           Data.String.Interpolate        ( i )
import qualified Data.Yaml                     as Y
import           Data.Yaml                      ( FromJSON(..)
                                                , (.:)
                                                , ToJSON(..)
                                                , (.=)
                                                )


import Infra.YamlFileIO
import           Domain.Action

data CheckoutState = CheckoutState CheckoutObject CheckoutObjectName
data CheckoutObject = ProjectObject | ContextObject
type CheckoutObjectName = Text

readCheckoutState :: YamlFilePath -> IO CheckoutState
readCheckoutState = readYamlFile (CheckoutState ProjectObject defaultProject)

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

