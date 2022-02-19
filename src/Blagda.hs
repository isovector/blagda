{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Blagda where

import           Blagda.Agda
import           Blagda.Latex
import           Blagda.Markdown
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Control.Monad.Writer
import qualified Data.ByteString.Lazy as LazyBS
import           Data.Digest.Pure.SHA
import           Data.Foldable
import           Data.Generics
import           Data.List
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.FilePath
import           Development.Shake.Forward (shakeArgsForward, forwardOptions, cacheAction)
import           Network.URI.Encode (decodeText)
import           System.Directory (createDirectoryIfMissing)
import qualified System.Directory as Dir
import           Text.DocTemplates
import           Text.HTML.TagSoup
import           Text.Pandoc
import           Text.Pandoc.Walk

-- | Parse an Agda module (in the final build directory) to find a list
-- of its definitions.
parseFileIdents :: Text -> Action (Map Text Reference, Map Text Text)
parseFileIdents mod =
  do
    let path = "_build/html1" </> Text.unpack mod <.> "html"
    need [ path ]
    traced ("parsing " ++ Text.unpack mod) do
      go mempty mempty . parseTags <$> Text.readFile path
  where
    go fwd rev (TagOpen "a" attrs:TagText name:TagClose "a":xs)
      | Just id <- lookup "id" attrs, Just href <- lookup "href" attrs
      , Just classes <- lookup "class" attrs
      , mod `Text.isPrefixOf` href, id `Text.isSuffixOf` href
      = go (Map.insert name (Reference href (Text.words classes)) fwd)
           (Map.insert href name rev) xs
      | Just classes <- lookup "class" attrs, Just href <- lookup "href" attrs
      , "Module" `elem` Text.words classes, mod `Text.isPrefixOf` href
      = go (Map.insert name (Reference href (Text.words classes)) fwd)
           (Map.insert href name rev) xs
    go fwd rev (_:xs) = go fwd rev xs
    go fwd rev [] = (fwd, rev)

--  Loads our type lookup table into memory
parseFileTypes :: () -> Action (Map Text Text)
parseFileTypes () = do
  need ["_build/all-pages.agda"]
  traced "loading types from iface" . runAgda $
    tcAndLoadPublicNames "_build/all-pages.agda"


agdaHTML :: Action [FilePath]
agdaHTML = do
  agda_files <- sort <$> getDirectoryFiles "site" ["**/*.agda", "**/*.lagda.md"]

  let
    toOut x | takeExtensions x == ".lagda.md"
            = moduleName (dropExtensions x) ++ " -- (text page)"
    toOut x = moduleName (dropExtensions x) ++ " -- (code only)"

  -- build the index
  writeFileLines "_build/all-pages.agda"
    $ "{-# OPTIONS --cubical #-}"
      : ["open import " ++ toOut x | x <- agda_files]
     ++ ["import " ++ x ++ " -- (builtin)" | x <- builtinModules]

  -- get agda html
  cacheAction @String @() "agda" $
    command [] "agda"
      [ "--html"
      , "--html-dir=_build/html0"
      , "--html-highlight=auto"
      , "--local-interfaces"
      , "--css=/css/agda-cats.css"
      , "_build/all-pages.agda"
      ]

  pure agda_files

