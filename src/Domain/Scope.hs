module Domain.Scope
  ( Scope(..)
  , ScopeType(..)
  , ScopeName
  )
where

data Scope = Scope ScopeType ScopeName
data ScopeType = ProjectScope | ContextScope
type ScopeName = Text

