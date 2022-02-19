{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ViewPatterns      #-}

module Blagda.Utils (getBuildPath, pattern Strs) where

import           Development.Shake.FilePath
import Data.Text (Text)
import Text.Pandoc (Inline(..))
import Control.Arrow ((&&&))

getBuildPath :: FilePath -> String -> FilePath -> FilePath
getBuildPath path ext x = "_build" </> path </> (dropExtension $ takeFileName x) <.> ext

pattern Strs :: Text -> [Inline]
pattern Strs ts <-
  ((id &&& id)
    ->
      ( all isStr -> True
      , foldMap fromStr -> ts
      )
  )

isStr :: Inline -> Bool
isStr (Str _) = True
isStr Space = True
isStr _ = False

fromStr :: Inline -> Text
fromStr (Str s) = s
fromStr Space = " "
fromStr _ = error "not a string"

