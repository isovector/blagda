{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Blagda.Rename where

import           Blagda.Markdown (renderHTML5)
import           Blagda.Types
import           Data.Functor ((<&>))
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.HTML.TagSoup (parseTags, Tag (TagOpen))

rename :: (forall contents. Post contents a -> FilePath) -> [Post Text a] -> [Post Text a]
rename f posts = do
  let rmap :: Map Text Text
      rmap =
        M.fromList $ do
          p <- posts
          pure (T.pack $ p_path p, T.pack $ f p)
  p <- posts
  let tags = parseTags $ p_contents p
  pure $ p
    { p_path = f p
    , p_contents = renderHTML5 $ tags <&> \case
        TagOpen "a" as ->
          TagOpen "a" $ as <&> replaceAttr "href" (flip M.lookup rmap)
        x -> x
    }

replaceAttr :: Text -> (Text -> Maybe Text) -> (Text, Text) -> (Text, Text)
replaceAttr attr f (k, v)
  | attr == k
  , Just v' <- f v
  = (attr, v')
replaceAttr _ _ kv = kv

