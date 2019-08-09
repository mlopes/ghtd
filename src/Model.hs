{-# LANGUAGE OverloadedStrings #-}

module Model
  ( Actions(..)
  )
where

import           Control.Applicative
import           Data.Text.Lazy                 ( Text )
import           Data.Yaml                      ( FromJSON(..)
                                                , (.:)
                                                , ToJSON(..)
                                                , (.=)
                                                )
import qualified Data.Yaml                     as Y
import           Prelude

data Actions =
  Actions
    { action :: Text
    , description :: Text
    , project :: Text
    , contexts :: [Text]
    }
  deriving (Show)

instance FromJSON Actions where
  parseJSON (Y.Object v) =
    Actions
      <$> v
      .:  "action"
      <*> v
      .:  "description"
      <*> v
      .:  "project"
      <*> v
      .:  "contexts"
  parseJSON _ = fail "Expected Object for Action value"

instance ToJSON Actions where
  toJSON a = Y.object
    [ "action" .= action a
    , "description" .= description a
    , "project" .= project a
    , "contexts" .= contexts a
    ]
