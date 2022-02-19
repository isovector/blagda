module Blagda.Diagrams where

import Blagda.Utils
import Control.Monad.Writer
import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Forward (cacheAction)


buildDiagrams :: Action ()
buildDiagrams = do
  diagrams <- fmap ("_build/diagrams" </>) <$> getDirectoryFiles "_build/diagrams" ["*.tex"]
  void $ forP diagrams $ \input -> do
    cacheAction input $
      command_ [Traced "build-diagram"] "sh" ["support/build-diagram.sh", getBuildPath "html" "svg" input, input]

