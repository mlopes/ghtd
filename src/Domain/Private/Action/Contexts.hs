{-# LANGUAGE OverloadedStrings #-}

module Domain.Private.Action.Contexts
  ( contextsFromString
  , contextsOfActions
  , contextsOfAnAction
  , defaultContexts
  )
where

import           Data.Text                      ( splitOn )
import           Data.List                      ( nub )

import           Domain.Action.Types

contextsFromString :: Text -> Contexts
contextsFromString = nub . splitOn ","

defaultContexts :: Contexts
defaultContexts = []

contextsOfActions :: Actions -> Contexts
contextsOfActions a = nub (a >>= contextsOfAnAction)

contextsOfAnAction :: Action -> Contexts
contextsOfAnAction (Action _ _ _ c _) = c

