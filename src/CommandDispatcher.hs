module CommandDispatcher
  ( dispatchCommand
  )
where

import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as L
import qualified Data.Text.Lazy.IO             as I
import           Data.UUID
import qualified Data.UUID.V4                  as UUID4

import           Action
import           CliCommand
import           Infra.YamlFileIO
import           Formatter

dispatchCommand :: Maybe CliCommand -> String -> IO ()
dispatchCommand Nothing filePath = do
  actions <- readActions filePath
  I.putStrLn (stringifyActions actions)
dispatchCommand (Just (Add description project contexts)) filePath = do
  action <- textNoDashesUUID4
  addAction filePath (Action action description project contexts ToDo)

dispatchCommand (Just x) _ = print x

textNoDashesUUID4 :: IO Text
textNoDashesUUID4 = stripDashes . uuidToLazyText <$> UUID4.nextRandom

uuidToLazyText :: UUID -> Text
uuidToLazyText = L.fromStrict . toText

stripDashes :: Text -> Text
stripDashes = L.filter ('-' /=)

