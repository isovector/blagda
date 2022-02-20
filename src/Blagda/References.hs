{-# LANGUAGE OverloadedStrings #-}

module Blagda.References where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.List
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           System.Environment
import           System.Exit
import           Text.HTML.TagSoup
import           Text.Pandoc.Definition
import           Text.Pandoc.JSON
import           Text.Pandoc.Walk


linkDocument :: Pandoc -> Pandoc
linkDocument (Pandoc meta blocks) =
  let hm = parseSymbolRefs blocks
  in Pandoc meta (walk (link hm) blocks)

link :: HashMap Text Reference -> Inline -> Inline
link hm inline@(Code (_, classes, kv) text)
  | isToBeLinked =
    case HashMap.lookup identifier hm of
      Just ref -> RawInline "html" (renderReference ref text)
      Nothing -> inline
 where
  classes' = map T.toLower classes

  isToBeLinked = ("agda" `elem` classes')
    && ("nolink" `notElem` classes')

  identifier =
    case lookup "ident" kv of
      Just id -> id
      _ -> text
link _ x = x

renderReference :: Reference -> Text -> Text
renderReference (Reference href cls) t =
 renderTags [ TagOpen "span" [("class", "Agda")]
 , TagOpen "a" [("href", href), ("class", cls)]
 , TagText t
 , TagClose "a"
 , TagClose "span"
 ]

data Reference =
 Reference { refHref :: Text
 , refClass :: Text
 }
 deriving (Eq, Show)

parseSymbolRefs :: [Block] -> HashMap Text Reference
parseSymbolRefs = go mempty . concat . mapMaybe getHTML where
  getHTML :: Block -> Maybe ([Tag Text])
  getHTML (RawBlock (Format x) xs)
    | x == "html" = Just (concatMap parseTags' (parseTags xs))
  getHTML (BlockQuote bs) = pure . concat $ mapMaybe getHTML bs
  getHTML (Div _ bs) = pure . concat $ mapMaybe getHTML bs
  getHTML _ = Nothing

  parseTags' (TagComment x) = parseTags x >>= parseTags'
  parseTags' t = pure t

  go :: HashMap Text Reference -> [Tag Text] -> HashMap Text Reference
  go map (TagOpen a meta:TagText t:TagClose a':xs)
    | a == "a"
    , a' == a
    , Just id <- lookup "id" meta
    , Just cls <- lookup "class" meta
    , Just href <- lookup "href" meta
    = go (addIfNotPresent t (Reference href cls) map) xs
    | otherwise = go map xs
    where
      tags = [ TagOpen "span" [("class", "Agda")], TagOpen "a" meta', TagText t, TagClose "a", TagClose "span" ]
      meta' = filter ((/= "id") . fst) meta
  go map (_:xs) = go map xs
  go map [] = map

addIfNotPresent :: Text -> v -> HashMap Text v -> HashMap Text v
addIfNotPresent = HashMap.insertWith (\_ old -> old)

