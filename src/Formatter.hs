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
import           Model

stringifyActions :: [Actions] -> Text
stringifyActions = textListToLines . actionListToTextList . indexList

indexList :: [a] -> [(Text, a)]
indexList = zip (fmap (T.pack . show) [1 ..])

textListToLines :: [Text] -> Text
textListToLines = T.intercalate "\n\n"

actionListToTextList :: [(Text, Actions)] -> [Text]
actionListToTextList = fmap (\x -> [i|#{fst x}. #{actionToText $ snd x}|])

actionToText :: Actions -> Text
actionToText a = T.intercalate
  "\n"
  [ [i|#{description a}|]
  , [i|Project: #{project a}|]
  , [i|Contexts: #{actionContextsToText a}|]
  , [i|(#{action a})|]
  ]

actionContextsToText :: Actions -> Text
actionContextsToText actions = T.intercalate ", " $ contexts actions
