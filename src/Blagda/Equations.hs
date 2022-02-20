{-# LANGUAGE OverloadedStrings #-}

module Blagda.Equations where

import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import           Text.HTML.TagSoup


hideSteps :: Bool -> [Tag Text] -> [Tag Text]
hideSteps has_eqn (to@(TagOpen "a" attrs):tt@(TagText t):tc@(TagClose "a"):rest)
  | Text.length t >= 1, Text.last t == '⟨', Just href <- lookup "href" attrs
  = [ TagOpen "span" [("class", "reasoning-step")]
    , TagOpen "span" [("class", "as-written " <> fromMaybe "" (lookup "class" attrs))]
    , to, tt, tc
    ] ++ go href rest
  where
    alternate = Text.init t
    go href (to@(TagOpen "a" attrs):tt@(TagText t):tc@(TagClose "a"):cs)
      | Text.length t >= 1
      , Text.head t == '⟩'
      , Just href' <- lookup "href" attrs
      , href' == href
      = [ to, tt, tc, TagClose "span"
        , TagOpen "span" [("class", "alternate " <> fromMaybe "" (lookup "class" attrs))]
        , TagText alternate
        , TagClose "span"
        , TagClose "span"
        ] ++ hideSteps True cs
    go href (c : cs) = c : go href cs
    go _ [] = []
hideSteps False (TagClose "html":cs) =
  [ TagOpen "style" []
  , TagText ".equations { display: none !important; }"
  , TagClose "style"
  , TagClose "html"
  ] ++ hideSteps True cs
hideSteps has_eqn (c : cs) = c : hideSteps has_eqn cs
hideSteps _ [] = []

