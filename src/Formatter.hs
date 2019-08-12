{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

module Formatter
  ( format
  )
where

import           Data.String.Interpolate        ( i )
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as T
import Action

format :: GhtdFormatable a => a -> Text
format = ghtdFormat

actionToText :: Action -> Text
actionToText (Action action description project contexts state) = T.intercalate
  "\n"
  [ [i|#{description} #{stateToCheckBox state}|]
  , [i|Project: #{project}|]
  , [i|Contexts: #{actionContextsToText contexts}|]
  , [i|(#{action})|]
  ]

indexList :: [a] -> [(Text, a)]
indexList = zip (fmap (T.pack . show) [1 :: Integer ..])

textListToLines :: [Text] -> Text
textListToLines = T.intercalate "\n\n"

actionListToTextList :: [(Text, Action)] -> [Text]
actionListToTextList = fmap (\x -> [i|#{fst x}. #{actionToText $ snd x}|])

actionContextsToText :: [Text] -> Text
actionContextsToText = T.intercalate ", "

stateToCheckBox :: ActionState -> Text
stateToCheckBox ToDo = [i|[ ]|]
stateToCheckBox Done = [i|[✓]|]
stateToCheckBox Cancelled = [i|[X]|]

class GhtdFormatable a where
  ghtdFormat ::  a -> Text

instance GhtdFormatable Action where
  ghtdFormat (Action action description project contexts state) = T.intercalate
    "\n"
    [ [i|#{description} #{stateToCheckBox state}|]
    , [i|Project: #{project}|]
    , [i|Contexts: #{actionContextsToText contexts}|]
    , [i|(#{action})|]
    ]

instance GhtdFormatable [Action] where
  ghtdFormat = textListToLines . actionListToTextList . indexList

