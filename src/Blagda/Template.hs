module Blagda.Template where

import           Blagda.Types
import           Data.Aeson
import           Data.Foldable (for_)
import           Data.Text (Text)
import qualified Data.Text as T
import           Development.Shake
import           Development.Shake.FilePath
import           Text.Mustache


writeTemplate :: ToJSON a => FilePath -> [Post Text a] -> Action ()
writeTemplate path posts = do
  Right template <- liftIO $ automaticCompile ["support/templates"] path
  for_ posts $ \post ->
    writeFile' ("_build/html" </> p_path post)
      $ T.unpack
      $ substitute template
      $ toJSON post

