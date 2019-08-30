{-# LANGUAGE OverloadedStrings #-}

module Domain.Scope
  ( Scope(..)
  , ScopeType(..)
  , ScopeName
  , scopedView
  )
where

import Domain.Action
import Domain.Action.Types

data Scope = Scope ScopeType ScopeName

data ScopeType = ProjectScope | ContextScope

type ScopeName = Text

type ScopeView = [(Text, Action)]

scopedView :: Scope -> Actions -> ScopeView
scopedView (Scope ProjectScope projectName) = indexList . filter (\x -> projectFromAction x == projectName)
scopedView (Scope ContextScope contextName) = indexList . filter (elem contextName . contextsOfAnAction)

indexList :: [a] -> [(Text, a)]
indexList = zip (fmap (pack . show) [1 :: Integer ..])

