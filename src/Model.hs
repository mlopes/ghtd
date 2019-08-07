{-# LANGUAGE OverloadedStrings #-}

module Model
  ( Actions(Actions)
  , description
  , project
  , contexts
  ) where

import Control.Applicative
import Data.Text.Lazy (Text)
import Data.Yaml (FromJSON(..), (.:))
import qualified Data.Yaml as Y
import Prelude

data Actions =
  Actions
       -- action :: Text
    { description :: Text
    , project :: Text
    , contexts :: [Text]
    }
  deriving (Show)

instance FromJSON Actions where
  parseJSON (Y.Object v) =
    Actions <$>
        -- v .: "action" <*>
    v .: "description" <*>
    v .: "project" <*>
    v .: "contexts"
  parseJSON _ = fail "Expected Object for Action value"
