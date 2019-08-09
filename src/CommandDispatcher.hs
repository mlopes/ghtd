module CommandDispatcher
  ( dispatchCommand
  )
where

import           Command
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as L
import           Data.UUID
import qualified Data.UUID.V4 as UUID4
import           Model
import           Lib

dispatchCommand :: Maybe Command -> String -> IO ()
dispatchCommand Nothing yamlFilePath = showActionsFromYaml yamlFilePath
dispatchCommand (Just (Add description project contexts)) yamlFilePath = do
  action <- textNoDashesUUID4
  addAction
    yamlFilePath
    Actions { action      = action
            , description = description
            , project     = project
            , contexts    = contexts
            }
dispatchCommand (Just x) _ = print x

textNoDashesUUID4 :: IO Text
textNoDashesUUID4 = stripDashes . uuidToLazyText <$> UUID4.nextRandom

uuidToLazyText :: UUID -> Text
uuidToLazyText = L.fromStrict . toText

stripDashes :: Text -> Text
stripDashes = L.filter ('-' /=)

