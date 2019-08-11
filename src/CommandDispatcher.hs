module CommandDispatcher
  ( dispatchCommand
  )
where

import           CliCommand
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as L
import           Data.UUID
import qualified Data.UUID.V4                  as UUID4
import           Model
import           Lib

dispatchCommand :: Maybe CliCommand -> String -> IO ()
dispatchCommand Nothing filePath = showActions filePath
dispatchCommand (Just (Add description project contexts)) filePath = do
  action <- textNoDashesUUID4
  addAction
    filePath
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

