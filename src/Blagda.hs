{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TypeApplications      #-}

module Blagda
  ( module Blagda
  , module Blagda.Template
  , module Blagda.Types
  , loadMarkdown
  , rename
  ) where

import Blagda.Agda
import Blagda.Markdown
import Blagda.Template
import Blagda.Rename (rename)
import Blagda.Types
import Blagda.Utils (pattern Strs)
import Data.List
import Data.Map.Lazy (Map)
import Data.Text (Text)
import Development.Shake
import Development.Shake.FilePath
import Text.Pandoc


--  Loads our type lookup table into memory
parseFileTypes :: () -> Action (Map Text Text)
parseFileTypes () = do
  need ["_build/all-pages.agda"]
  traced "loading types from iface" . runAgda $
    tcAndLoadPublicNames "_build/all-pages.agda"


agdaHTML :: FilePath -> Action [FilePath]
agdaHTML dir = do
  agda_files <- sort <$> getDirectoryFiles dir ["**/*.agda", "**/*.lagda.md"]

  let
    toOut x | takeExtensions x == ".lagda.md"
            = moduleName (dropExtensions x) ++ " -- (text page)"
    toOut x = moduleName (dropExtensions x) ++ " -- (code only)"

  -- build the index
  writeFileLines "_build/all-pages.agda"
    $ ["open import " ++ toOut x | x <- agda_files]
     ++ ["import " ++ x ++ " -- (builtin)" | x <- builtinModules]

  -- get agda html
  -- cacheAction @String @() "agda" $

  command @() [] "agda"
    [ "--html"
    , "--html-dir=_build/html0"
    , "--html-highlight=auto"
    , "--local-interfaces"
    , "--css=/css/agda-cats.css"
    , "_build/all-pages.agda"
    ]

  pure agda_files


parseMetaString :: MetaValue -> Maybe Text
parseMetaString (MetaString txt) = Just txt
parseMetaString (MetaInlines (Strs txt)) = Just txt
parseMetaString _ = Nothing

