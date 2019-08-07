{-# LANGUAGE OverloadedStrings #-}

module Formatter
    (
       stringifyActions
    ) where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Model

stringifyActions :: [Actions] -> Text
stringifyActions = textListToLines . actionListToTextList . indexList

indexList :: [a] -> [(Int, a)]
indexList l = zip [1..(length l)] l

textListToLines :: [Text] -> Text
textListToLines = T.intercalate "\n"

actionListToTextList :: [(Int, Actions)] -> [Text]
actionListToTextList l = map (\x -> let index = T.pack . show $ fst x
                                        currentAction = snd x in
                                    T.concat[index, ". ",  description currentAction, " - ", project currentAction, " (", actionContextsToText currentAction, ")"]) l

actionContextsToText :: Actions -> Text
actionContextsToText actions = T.intercalate ", " $ contexts actions
