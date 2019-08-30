{-# LANGUAGE OverloadedStrings #-}

module Domain.Private.Action.Project
  ( projectsFromActions
  , projectFromAction
  , defaultProject
  )
where

import           Data.List                      ( nub )

import           Domain.Action.Types

defaultProject :: Project
defaultProject = "Inbox"

projectsFromActions :: Actions -> Projects
projectsFromActions = nub . fmap projectFromAction

projectFromAction :: Action -> Project
projectFromAction (Action _ _ p _ _) = p

