{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Blagda.Markdown where

import           Blagda.Agda
import           Blagda.Latex
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

data Reference
  = Reference { refHref :: Text
              , refClasses :: [Text]
              }
  deriving (Eq, Show)

buildMarkdown :: String -> FilePath -> FilePath -> Action FilePath
buildMarkdown commit input output = do
  let
    templateName = "support/web/template.html"
    modname = moduleName (dropDirectory1 (dropDirectory1 (dropExtension input)))

  -- need [templateName, input]

  -- modulePath <- fmap aom_fp $ findModule modname
  let
    -- TODO(sandy):
    permalink = commit </> mempty

    title
      | length modname > 24 = '…':reverse (take 24 (reverse modname))
      | otherwise = modname

  Pandoc meta markdown <- liftIO do
    contents <- Text.readFile input
    either (fail . show) pure =<< runIO do
      md <- readMarkdown def { readerExtensions = getDefaultExtensions "markdown" } [(input, contents)]
      pure md
      -- applyFilters def [JSONFilter "agda-reference-filter"] ["html"] md

  let
    htmlInl = RawInline (Format "html")

    -- | Replace any expression $foo$-bar with <span ...>$foo$-bar</span>, so that
    -- the equation is not split when word wrapping.
    patchInlines (m@Math{}:s@(Str txt):xs)
      | not (Text.isPrefixOf " " txt)
      = htmlInl "<span style=\"white-space: nowrap;\">" : m : s : htmlInl "</span>"
      : patchInlines xs
    patchInlines (x:xs) = x:patchInlines xs
    patchInlines [] = []

    -- Make all headers links, and add an anchor emoji.
    patchBlock (Header i a@(ident, _, _) inl) = pure $ Header i a
      $ htmlInl (Text.concat ["<a href=\"#", ident, "\" class=\"header-link\">"])
      : inl
      ++ [htmlInl "<span class=\"header-link-emoji\">🔗</span></a>"]
    -- Replace quiver code blocks with a link to an SVG file, and depend on the SVG file.
    patchBlock (CodeBlock (id, classes, attrs) contents) | "quiver" `elem` classes = do
      let
        digest = showDigest . sha1 . LazyBS.fromStrict $ Text.encodeUtf8 contents
        title = fromMaybe "commutative diagram" (lookup "title" attrs)
      writeFile' ("_build/diagrams" </> digest <.> "tex") $ Text.unpack contents

      pure $ Div ("", ["diagram-container"], [])
        [ Plain [ Image (id, "diagram":classes, attrs) [] (Text.pack (digest <.> "svg"), title) ]
        ]
    patchBlock h = pure h

    patchInline (Math DisplayMath contents) = htmlInl <$> buildLatexEqn True contents
    patchInline (Math InlineMath contents) = htmlInl <$> buildLatexEqn False contents
    patchInline h = pure h

    mStr = MetaString . Text.pack
    patchMeta = Meta . Map.insert "title" (mStr title) . Map.insert "source" (mStr permalink) . unMeta

  liftIO $ Dir.createDirectoryIfMissing False "_build/diagrams"

  markdown <- pure . walk patchInlines . Pandoc (patchMeta meta) $ markdown
  markdown <- walkM patchInline markdown
  markdown <- walkM patchBlock markdown

  text <- liftIO $ either (fail . show) pure =<< runIO do
    template <- getTemplate templateName >>= runWithPartials . compileTemplate templateName
                >>= either (throwError . PandocTemplateError . Text.pack) pure
    let
      context = Context $ Map.fromList
                [ (Text.pack "is-index", toVal (modname == "index"))
                -- , (Text.pack "authors", toVal authors')
                ]
      options = def { writerTemplate = Just template
                    , writerTableOfContents = True
                    , writerVariables = context
                    , writerExtensions = getDefaultExtensions "html" }
    writeHtml5String options markdown

  -- TODO(sandy): dont use mempty
  -- tags <- traverse (parseAgdaLink $ const $ pure (mempty, mempty)) (parseTags text)
  writeFile' output $ Text.unpack text -- (renderHTML5 tags)
  pure output

  -- command_ [] "agda-fold-equations" [output]


-- | Possibly interpret an <a href="agda://"> link to be a honest-to-god
-- link to the definition.
parseAgdaLink :: (Text -> Action (Map Text Reference, Map Text Text))
                 -> Tag Text -> Action (Tag Text)
parseAgdaLink fileIds (TagOpen "a" attrs)
  | Just href <- lookup "href" attrs, Text.pack "agda://" `Text.isPrefixOf` href = do
    let
      href' = Text.splitOn "#" $ Text.drop (Text.length "agda://") href
      cont mdl ident = do
        (idMap, _) <- fileIds mdl
        case Map.lookup ident idMap of
          Just (Reference href'' _) -> do
            pure (TagOpen "a" (emplace [("href", href'')] attrs))
          _ -> error $ "Could not compile Agda link: " ++ show href'
    case href' of
      [mdl] -> cont mdl mdl
      [mdl, ident] -> cont mdl (decodeText ident)
      _ -> error $ "Could not parse Agda link: " ++ show href
parseAgdaLink _ x = pure x

emplace :: Eq a => [(a, b)] -> [(a, b)] -> [(a, b)]
emplace ((p, x) : xs) ys = (p, x) : emplace xs (filter ((/= p) . fst) ys)
emplace [] ys = ys

-- | Lookup an identifier given a module name and ID within that module,
-- returning its type.
addLinkType :: (Text -> Action (Map Text Reference, Map Text Text)) -- ^ Lookup an ident from a module name and location
             -> (() -> Action (Map Text Text)) -- ^ Lookup a type from a module name and ident
             -> Tag Text -> Action (Tag Text)
addLinkType fileIds fileTys tag@(TagOpen "a" attrs)
  | Just href <- lookup "href" attrs
  , [mdl, _] <- Text.splitOn ".html#" href = do
    ty <- resolveId mdl href <$> fileIds mdl <*> fileTys ()
    pure case ty of
      Nothing -> tag
      Just ty -> TagOpen "a" (emplace [("data-type", ty)] attrs)

    where
      resolveId _ href (_, _) types = Map.lookup href types
addLinkType _ _ x = pure x

