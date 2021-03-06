{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

module Formatter
  ( format
  , GhtdFormatable
  )
where

import           Data.String.Interpolate        ( i )

import           Domain.Action.Types
import           Domain.Scope

format :: GhtdFormatable a => a -> Text
format = ghtdFormat

textListToLines :: [Text] -> Text
textListToLines = intercalate "\n\n"

actionListToTextList :: [(Text, Action)] -> [Text]
actionListToTextList = fmap (\x -> [i|#{fst x}. #{format $ snd x}|])

actionContextsToText :: [Text] -> Text
actionContextsToText = intercalate ", "

stateToCheckBox :: ActionState -> Text
stateToCheckBox ToDo      = [i|[ ]|]
stateToCheckBox Done      = [i|[✓]|]
stateToCheckBox Cancelled = [i|[X]|]

class GhtdFormatable a where
  ghtdFormat ::  a -> Text

instance GhtdFormatable Action where
  ghtdFormat (Action action description project contexts state) = intercalate
    "\n"
    [ [i|#{description} #{stateToCheckBox state}|]
    , [i|Project: #{project}|]
    , [i|Contexts: #{actionContextsToText contexts}|]
    , [i|(#{action})|]
    ]

instance GhtdFormatable Projects where
  ghtdFormat = intercalate "\n"

instance GhtdFormatable ScopeView where
  ghtdFormat = textListToLines . actionListToTextList

