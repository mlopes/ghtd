module Infra.Printer
  ( ghtdPrint
  )
where

import           Formatter

ghtdPrint :: GhtdFormatable a => a -> IO ()
ghtdPrint = putStrLn . format
