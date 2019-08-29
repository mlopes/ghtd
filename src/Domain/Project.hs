{-# LANGUAGE OverloadedStrings #-}

module Domain.Project
  ( Project
  , defaultProject
  )
where

type Project = Text

defaultProject :: Project
defaultProject = "Inbox"

