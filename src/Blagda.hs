{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TypeApplications      #-}

module Blagda where

import           Blagda.Agda
import           Blagda.Markdown
import           Blagda.Utils (pattern Strs)
import           Data.List
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Time (UTCTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Data.Time.Format (parseTimeM)
import           Development.Shake
import           Development.Shake.FilePath
import           Development.Shake.Forward (cacheAction)
import           Text.DocTemplates
import           Text.HTML.TagSoup
import           Text.Pandoc


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


data Article = Article
  { a_title    :: Text
  , a_datetime :: UTCTime
  , a_meta     :: Meta
  }
  deriving (Eq, Ord, Show)

instance ToContext Text Article where
  toVal (Article title date meta) = MapVal $ Context $ Map.fromList
    [ ("title", toVal title)
    , ("datetime", toVal $ Text.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%i" date)
    ]

parseMetaString :: MetaValue -> Maybe Text
parseMetaString (MetaString txt) = Just txt
parseMetaString (MetaInlines (Strs txt)) = Just txt
parseMetaString _ = Nothing

parseHeader :: Meta -> Maybe Article
parseHeader meta@(Meta m) =
  Article
    <$> (parseMetaString =<< Map.lookup "title" m)
    <*> ( parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M"
            . Text.unpack =<< parseMetaString
                          =<< Map.lookup "date" m)
    <*> pure meta

