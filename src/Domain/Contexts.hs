{-# LANGUAGE OverloadedStrings #-}

module Domain.Contexts
  ( contextsFromString
  , defaultContexts
  , Context
  , Contexts
  )
where

import           Data.Text                      ( splitOn )
import           Data.List                      ( nub )

type Context = Text
type Contexts = [Context]

contextsFromString :: Text -> Contexts
contextsFromString = nub . splitOn ","

defaultContexts :: Contexts
defaultContexts = []

