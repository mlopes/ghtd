{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Formatter
  ( stringifyActions
  , actionToText
  )
where

import           Data.String.Interpolate        ( i )
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as T
import Action

stringifyActions :: [Action] -> Text
stringifyActions = textListToLines . actionListToTextList . indexList

indexList :: [a] -> [(Text, a)]
indexList = zip (fmap (T.pack . show) [1 ..])

textListToLines :: [Text] -> Text
textListToLines = T.intercalate "\n\n"

actionListToTextList :: [(Text, Action)] -> [Text]
actionListToTextList = fmap (\x -> [i|#{fst x}. #{actionToText $ snd x}|])

actionToText :: Action -> Text
actionToText (Action action description project contexts state) = T.intercalate
  "\n"
  [ [i|#{description}|]
  , [i|Project: #{project}|]
  , [i|Contexts: #{actionContextsToText contexts}|]
  , [i|(#{action})|]
  ]

actionContextsToText :: [Text] -> Text
actionContextsToText = T.intercalate ", "

