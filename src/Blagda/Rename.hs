{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE BangPatterns #-}

module Blagda.Rename where

import           Agda.Utils.Functor ((<&>))
import           Blagda.Markdown (renderHTML5)
import           Blagda.Types
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.HTML.TagSoup (Tag(TagOpen), parseTags)
import           Text.Pandoc.Definition
import           Text.Pandoc.Walk (walk)

rename :: (FilePath -> FilePath) -> [Post Pandoc a] -> [Post Pandoc a]
rename f posts = do
  let rn fp =
        let segs = T.split (flip elem ['?', '#']) fp
         in T.intercalate "#" $ onHead segs $ T.pack . f . T.unpack
  p <- posts
  let res = walk (replaceBlock rn) $ walk (replaceInline rn) $ p_contents p
  pure $ p
    { p_path = f $ p_path p
    , p_contents = res
    }

onHead :: [a] -> (a -> a) -> [a]
onHead [] _ = []
onHead (a : as) faa = faa a : as

replaceInline :: (Text -> Text) -> Inline -> Inline
replaceInline f (Link attrs txt (url, tg))
  = Link attrs txt (f url, tg)
replaceInline f (RawInline (Format "html") t)
  = RawInline "html" $ replaceHtml f t
replaceInline _ i = i

replaceBlock :: (Text -> Text) -> Block -> Block
replaceBlock f (RawBlock (Format "html") t) = RawBlock "html" $ replaceHtml f t
replaceBlock _ i = i

replaceHtml :: (Text -> Text) -> Text -> Text
replaceHtml f t =
  let tags = parseTags t
   in renderHTML5 $ tags <&> \case
        TagOpen "a" as -> TagOpen "a" $ as <&> replaceAttr f
        x -> x

replaceAttr :: (Text -> Text) -> (Text, Text) -> (Text, Text)
replaceAttr f ("href", v) = ("href", f v)
replaceAttr _ kv = kv

