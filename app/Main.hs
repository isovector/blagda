{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import           Blagda.Agda
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


newtype LatexEquation = LatexEquation (Bool, Text) -- TODO: Less lazy instance
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult LatexEquation = Text

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
      liftIO $ Text.writeFile ("_build/diagrams" </> digest <.> "tex") contents
      tell ["_build/html" </> digest <.> "svg"]

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
  (markdown, dependencies) <- runWriterT $ walkM patchBlock markdown
  -- need dependencies

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
  liftIO $ Text.writeFile output text -- (renderHTML5 tags)
  pure output

  -- command_ [] "agda-fold-equations" [output]



buildLatexEqn :: Bool -> Text -> Action Text
buildLatexEqn display tex = cacheAction (display, tex) $ do
  need [".macros"]

  let args = ["-f", ".macros", "-t"] ++ ["-d" | display]
      stdin = LazyBS.fromStrict $ Text.encodeUtf8 tex
  Stdout out <- command [StdinBS stdin] "katex" args
  pure . Text.stripEnd . Text.decodeUtf8 $ out


renderHTML5 :: [Tag Text] -> Text
renderHTML5 = renderTagsOptions renderOptions{ optMinimize = min } where
  min = flip elem ["br", "meta", "link", "img", "hr"]

main :: IO ()
main =
  shakeArgsForward
    (forwardOptions $ shakeOptions
      { shakeFiles="_build"
      , shakeLintInside=["site"]
      , shakeChange=ChangeDigest
      , shakeVersion = "2"
      }) $ do

  liftIO $ createDirectoryIfMissing True "_build/html0"
  liftIO $ createDirectoryIfMissing True "_build/html1"
  liftIO $ createDirectoryIfMissing True "_build/html"

  agda_files <- sort <$> getDirectoryFiles "site" ["**/*.agda", "**/*.lagda.md"]
  md_files' <- getDirectoryFiles "site" ["**/*.md"]
  let md_files = Set.toList $ Set.fromList md_files' Set.\\ Set.fromList agda_files

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

  commit <- gitCommit

  md0   <- sort . fmap ("_build/html0" </>) <$> getDirectoryFiles "_build/html0" ["*.md"]
  html0 <- sort . fmap ("_build/html0" </>) <$> getDirectoryFiles "_build/html0" ["*.html"]

  let getBuildPath path ext x = "_build" </> path </> (dropExtension $ takeFileName x) <.> ext
      getHtml1Path = getBuildPath "html1" "html"
      getHtmlPath = getBuildPath "html" "html"


  md0' <- forP (fmap ("site" </>) md_files <> md0) $ \md ->
    buildMarkdown commit md $ getHtml1Path md

  void $ forP html0 $ \html ->
    liftIO $ Dir.copyFile html $ getHtml1Path html

  html1 <- sort . fmap ("_build/html1" </>) <$> getDirectoryFiles "_build/html1" ["*.html"]

  void $ forP html1 $ \input -> do
    let out = getHtmlPath input
    text <- liftIO $ Text.readFile input
    -- tags <- traverse (addLinkType undefined undefined) (parseTags text)
    tags <- pure $ parseTags text
    traverse_ (checkMarkup (takeFileName out)) tags
    liftIO $ Text.writeFile out $ renderHTML5 tags

  diagrams <- fmap ("_build/diagrams" </>) <$> getDirectoryFiles "_build/diagrams" ["*.tex"]
  void $ forP diagrams $ \input -> do
    cacheAction input $
      command_ [Traced "build-diagram"] "sh" ["support/build-diagram.sh", getBuildPath "html" "svg" input, input]

  sass <- getDirectoryFiles "" ["support/web/*.scss"]
  void $ forP sass $ \input ->
    cacheAction input $
      command_ [] "sass" [input, getBuildPath "html/css" "css" input]

  statics <- getDirectoryFiles "support/static" ["**/*"]
  void $ forP statics $ \filepath ->
    copyFileChanged ("support/static" </> filepath) ("_build/html" </> filepath)


-- | Possibly interpret an <a href="agda://"> link to be a honest-to-god
-- link to the definition.
parseAgdaLink :: (Text -> Action (Map Text Reference, Map Text Text))
                 -> Tag Text -> Action (Tag Text)
parseAgdaLink fileIds tag@(TagOpen "a" attrs)
  | Just href <- lookup "href" attrs, Text.pack "agda://" `Text.isPrefixOf` href = do
    href <- pure $ Text.splitOn "#" (Text.drop (Text.length "agda://") href)
    let
      cont mod ident = do
        (idMap, _) <- fileIds mod
        case Map.lookup ident idMap of
          Just (Reference href classes) -> do
            pure (TagOpen "a" (emplace [("href", href)] attrs))
          _ -> error $ "Could not compile Agda link: " ++ show href
    case href of
      [mod] -> cont mod mod
      [mod, ident] -> cont mod (decodeText ident)
      _ -> error $ "Could not parse Agda link: " ++ show href
parseAgdaLink _ x = pure x

emplace :: Eq a => [(a, b)] -> [(a, b)] -> [(a, b)]
emplace ((p, x):xs) ys = (p, x):emplace xs (filter ((/= p) . fst) ys)
emplace [] ys = ys

-- | Lookup an identifier given a module name and ID within that module,
-- returning its type.
addLinkType :: (Text -> Action (Map Text Reference, Map Text Text)) -- ^ Lookup an ident from a module name and location
             -> (() -> Action (Map Text Text)) -- ^ Lookup a type from a module name and ident
             -> Tag Text -> Action (Tag Text)
addLinkType fileIds fileTys tag@(TagOpen "a" attrs)
  | Just href <- lookup "href" attrs
  , [mod, _] <- Text.splitOn ".html#" href = do
    ty <- resolveId mod href <$> fileIds mod <*> fileTys ()
    pure case ty of
      Nothing -> tag
      Just ty -> TagOpen "a" (emplace [("data-type", ty)] attrs)

    where
      resolveId mod href (_, ids) types = Map.lookup href types
addLinkType _ _ x = pure x

checkMarkup :: FilePath -> Tag Text -> Action ()
checkMarkup file (TagText txt)
  |  "<!--" `Text.isInfixOf` txt || "<!–" `Text.isInfixOf` txt
  || "-->" `Text.isInfixOf` txt  || "–>" `Text.isInfixOf` txt
  = fail $ "[WARN] " ++ file ++ " contains misplaced <!-- or -->"
checkMarkup _ _ = pure ()


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


gitCommit :: Action String
gitCommit = do
  Stdout t <- command [] "git" ["rev-parse", "--verify", "HEAD"]
  pure (head (lines t))

--  Loads our type lookup table into memory
parseFileTypes :: () -> Action (Map Text Text)
parseFileTypes () = do
  need ["_build/all-pages.agda"]
  traced "loading types from iface" . runAgda $
    tcAndLoadPublicNames "_build/all-pages.agda"

