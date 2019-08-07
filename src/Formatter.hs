{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Formatter
  ( stringifyActions
  ) where

import Data.String.Interpolate (i)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Model

stringifyActions :: [Actions] -> Text
stringifyActions = textListToLines . actionListToTextList . indexList

indexList :: [a] -> [(Int, a)]
indexList l = zip [1 .. (length l)] l

textListToLines :: [Text] -> Text
textListToLines = T.intercalate "\n\n"

actionListToTextList =
  map
    (\x ->
       let index = T.pack . show $ fst x
           currentAction = snd x
        in T.intercalate
             "\n"
             [ [i|#{index}. #{description currentAction}|]
             , [i|Project: #{project currentAction}|]
             , [i|Contexts: #{actionContextsToText currentAction}|]
             , [i|(#{action currentAction})|]
             ])

actionContextsToText :: Actions -> Text
actionContextsToText actions = T.intercalate ", " $ contexts actions
