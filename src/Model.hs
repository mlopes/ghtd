{-# LANGUAGE OverloadedStrings #-}

module Model
    (
      Actions(Actions)
      , description
      , project
      , contexts
    ) where

import Data.Text.Lazy (Text)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import Control.Applicative
import Prelude -- Ensure Applicative is in scope and we have no warnings, before/after AMP.

data Actions =
    Actions {
       -- action :: Text
      description :: Text
      , project :: Text
      , contexts :: [Text]
    } deriving Show

instance FromJSON Actions where
    parseJSON (Y.Object v) =
        Actions <$>
        -- v .: "action" <*>
        v .: "description" <*>
        v .: "project" <*>
        v .: "contexts"
    parseJSON _ = fail "Expected Object for Action value"
