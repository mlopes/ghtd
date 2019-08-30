module Domain.Scope
  ( Scope(..)
  , ScopeType(..)
  , ScopeName
  , ScopeView
  , ScopeViewer
  , scopedView
  )
where

import           Domain.Action
import           Domain.Action.Types

data Scope = Scope ScopeType ScopeName

data ScopeType = ProjectScope | ContextScope

type ScopeName = Text

type ScopeView = [(Text, Action)]

type ScopeViewer = Actions -> ScopeView

scopedView :: Scope -> ScopeViewer
scopedView (Scope ProjectScope projectName) =
  indexList . filter (\x -> projectFromAction x == projectName)
scopedView (Scope ContextScope contextName) =
  indexList . filter (elem contextName . contextsOfAnAction)

indexList :: [a] -> [(Text, a)]
indexList = zip (fmap (pack . show) [1 :: Integer ..])

