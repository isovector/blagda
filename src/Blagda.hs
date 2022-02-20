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
    $ ["open import " ++ toOut x | x <- agda_files]
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
  { a_slug     :: Text
  , a_title    :: Text
  , a_datetime :: UTCTime
  , a_meta     :: Meta
  }
  deriving (Eq, Ord, Show)

instance ToContext Text Article where
  toVal (Article slug title date meta) = MapVal $ Context $ Map.fromList
    [ ("slug", toVal slug)
    , ("title", toVal title)
    , ("datetime", toVal $ Text.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M" date)
    , ("date", toVal $ Text.pack $ formatTime defaultTimeLocale "%Y-%m-%d" date)
    ]

parseMetaString :: MetaValue -> Maybe Text
parseMetaString (MetaString txt) = Just txt
parseMetaString (MetaInlines (Strs txt)) = Just txt
parseMetaString _ = Nothing

parseHeader :: Text -> Meta -> Maybe Article
parseHeader slug meta@(Meta m) =
  Article
    <$> pure slug
    <*> (parseMetaString =<< Map.lookup "title" m)
    <*> ( parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M"
            . Text.unpack =<< parseMetaString
                          =<< Map.lookup "date" m)
    <*> pure meta

