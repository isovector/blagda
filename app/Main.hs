{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad.IO.Class
import Control.Monad.Error.Class
import Control.Monad.Writer
import Control.Concurrent
import Control.Monad

import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Map.Lazy as Map
import qualified Data.Text as Text
import qualified Data.Set as Set
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Traversable
import Data.Digest.Pure.SHA
import Data.Map.Lazy (Map)
import Data.Generics
import Data.Function (on)
import Data.Foldable
import Data.Either
import Data.Maybe
import Data.Text (Text)
import Data.List

import Development.Shake.FilePath
import Development.Shake.Classes
import Development.Shake

import Network.URI.Encode (decodeText)

import qualified System.Directory as Dir
import System.IO.Unsafe
import System.Console.GetOpt
import System.IO

import Text.HTML.TagSoup
import Text.DocTemplates

import Text.Pandoc.Filter
import Text.Pandoc.Walk
import Text.Pandoc

import Agda.Syntax.Translation.AbstractToConcrete (abstractToConcrete_)
import Agda.Interaction.FindFile (SourceFile(..), rootNameModule)
import Agda.TypeChecking.Pretty (PrettyTCM(prettyTCM))
import Agda.Syntax.Translation.InternalToAbstract ( Reify(reify) )
import Agda.Syntax.Internal (Type, Dom, domName)
import Agda.TypeChecking.Serialise (decodeFile)
import qualified Agda.Interaction.FindFile as Agda
import qualified Agda.Utils.Maybe.Strict as S
import qualified Agda.Syntax.Concrete as Con
import Agda.TypeChecking.Monad.Options
import Agda.TypeChecking.Monad.Base
import Agda.Syntax.Abstract.Views
import Agda.Interaction.Options
import Agda.Interaction.Imports
import Agda.TypeChecking.Monad
import Agda.Syntax.Scope.Base
import Agda.Syntax.Abstract
import Agda.Syntax.Position
import Agda.Utils.FileName
import Agda.Syntax.Common
import Agda.Utils.Pretty
import Agda.Syntax.Info
import Development.Shake.Forward (shakeArgsForward, forwardOptions, cache, cacheAction)
import System.Directory (createDirectoryIfMissing)

newtype LatexEquation = LatexEquation (Bool, Text) -- TODO: Less lazy instance
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult LatexEquation = Text

data Reference
  = Reference { refHref :: Text
              , refClasses :: [Text]
              }
  deriving (Eq, Show)

moduleName :: FilePath -> String
moduleName = intercalate "." . splitDirectories

data AgdaOrMarkdown
  = Agda { aom_fp :: FilePath }
  | Markdown { aom_fp :: FilePath }

findModule :: MonadIO m => String -> m AgdaOrMarkdown
findModule modname = do
  let toPath '.' = '/'
      toPath c = c
  let modfile = "src" </> map toPath modname

  is_lagda <- liftIO $ Dir.doesFileExist (modfile <.> "lagda.md")
  is_agda <- liftIO $ Dir.doesFileExist (modfile <.> "agda")
  is_md <- liftIO $ Dir.doesFileExist (modfile <.> "md")
  pure $
    case (is_lagda, is_agda, is_md) of
      (True, _, _) -> Agda $ modfile <.> "lagda.md"
      (_, True, _) -> Agda $ modfile <.> "agda"
      (_, _, True) -> Markdown $ modfile <.> "md"
      _ -> error $ "File must be one of .lagda.md, .agda, or .md" <> modname

buildMarkdown :: String -> FilePath -> FilePath -> Action FilePath
buildMarkdown gitCommit input output = do
  let
    templateName = "support/web/template.html"
    modname = moduleName (dropDirectory1 (dropDirectory1 (dropExtension input)))

  -- need [templateName, input]

  -- modulePath <- fmap aom_fp $ findModule modname
  let
    -- TODO(sandy):
    permalink = gitCommit </> mempty

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
      , shakeLintInside=["src"]
      , shakeChange=ChangeDigest
      , shakeVersion = "2"
      }) $ do

  liftIO $ createDirectoryIfMissing True "_build/html0"
  liftIO $ createDirectoryIfMissing True "_build/html1"
  liftIO $ createDirectoryIfMissing True "_build/html"

  agda_files <- sort <$> getDirectoryFiles "src" ["**/*.agda", "**/*.lagda.md"]
  md_files' <- getDirectoryFiles "src" ["**/*.md"]
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

  md0   <- sort . fmap ("_build/html0" </>) <$> getDirectoryFiles "_build/html0" ["**/*.md"]
  html0 <- sort . fmap ("_build/html0" </>) <$> getDirectoryFiles "_build/html0" ["**/*.html"]

  let getBuildPath path ext x = "_build" </> path </> (dropExtension $ takeFileName x) <.> ext
      getHtml1Path = getBuildPath "html1" "html"
      getHtmlPath = getBuildPath "html" "html"


  md0' <- forP (fmap ("src" </>) md_files <> md0) $ \md ->
    buildMarkdown commit md $ getHtml1Path md

  void $ forP html0 $ \html ->
    liftIO $ Dir.copyFile html $ getHtml1Path html

  html1 <- sort . fmap ("_build/html1" </>) <$> getDirectoryFiles "_build/html1" ["**/*.html"]

  void $ forP html1 $ \input -> do
    let out = getHtmlPath input
    text <- liftIO $ Text.readFile input
    -- tags <- traverse (addLinkType undefined undefined) (parseTags text)
    tags <- pure $ parseTags text
    traverse_ (checkMarkup (takeFileName out)) tags
    liftIO $ Text.writeFile out $ renderHTML5 tags

  -- "_build/html/*.svg" %> \out -> do
  --   let inp = "_build/diagrams" </> takeFileName out -<.> "tex"
  --   need [inp]
  --   command_ [Traced "build-diagram"] "sh"
  --     ["support/build-diagram.sh", out, inp]
  --   removeFilesAfter "." ["rubtmp*"]

  -- "_build/html/css/*.css" %> \out -> do
  --   let inp = "support/web/" </> takeFileName out -<.> "scss"
  --   need [inp]
  --   command_ [] "sassc" [inp, out]

  -- "_build/html/favicon.ico" %> \out -> do
  --   need ["support/favicon.ico"]
  --   copyFile' "support/favicon.ico" out

  -- "_build/html/static/**/*" %> \out -> do
  --   let inp = "support/" </> dropDirectory1 (dropDirectory1 out)
  --   need [inp]
  --   traced "copying" $ Dir.copyFile inp out

  -- "_build/html/*.js" %> \out -> do
  --   let inp = "support/web" </> takeFileName out
  --   need [inp]
  --   traced "copying" $ Dir.copyFile inp out

  -- versioned 3 $ phony "all" do
  --   need ["_build/all-pages.agda"]
  --   need ["_build/all-posts.md"]
  --   files <- filter ("open import" `isPrefixOf`) . lines <$> readFile' "_build/all-pages.agda"
  --   posts <- fmap lines $ readFile' "_build/all-posts.md"
  --   need $ "_build/html/all-pages.html"
  --        : [ "_build/html" </> (words file !! 2) <.> "html"
  --          | file <- files
  --          ]
  --   need $ [ "_build/html" </> file <.> "html"
  --          | file <- posts
  --          ]

  --   f1 <- getDirectoryFiles "support" ["**/*.scss"] >>= \files -> pure ["_build/html/css/" </> takeFileName f -<.> "css" | f <- files]
  --   f2 <- getDirectoryFiles "support" ["**/*.js"] >>= \files -> pure ["_build/html/" </> takeFileName f | f <- files]
  --   f3 <- getDirectoryFiles "support/static/" ["**/*"] >>= \files ->
  --     pure ["_build/html/static" </> f | f <- files]
  --   f4 <- getDirectoryFiles "_build/html0" ["Agda.*.html"] >>= \files ->
  --     pure ["_build/html/" </> f | f <- files]
  --   need $ "_build/html/favicon.ico":(f1 ++ f2 ++ f3 ++ f4)

  -- phony "clean" do
  --   removeFilesAfter "_build" ["html0/*", "html/*", "diagrams/*", "*.agda", "*.md", "*.html"]

  -- phony "really-clean" do
  --   need ["clean"]
  --   removeFilesAfter "_build" ["**/*.agdai", "*.lua"]

  -- versioned 1 do
  --   _ <- addOracleCache \(LatexEquation (display, tex)) -> do
  --     need [".macros"]

  --     let args = ["-f", ".macros", "-t"] ++ ["-d" | display]
  --         stdin = LazyBS.fromStrict $ Text.encodeUtf8 tex
  --     Stdout out <- command [StdinBS stdin] "katex" args
  --     pure . Text.stripEnd . Text.decodeUtf8 $ out

  --   pure ()

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

runAgda :: (String -> TCMT IO a) -> IO a
runAgda k = do
  e <- runTCMTop $ do
    p <- setupTCM
    k p
  case e of
    Left s -> error (show s)
    Right x -> pure x

setupTCM :: TCMT IO String
setupTCM = do
  absp <- liftIO $ absolute "./src"
  setCommandLineOptions' absp defaultOptions{optLocalInterfaces = True}
  pure (filePath absp)

killDomainNames :: Type -> Type
killDomainNames = everywhere (mkT unDomName) where
  unDomName :: Dom Type -> Dom Type
  unDomName m = m{ domName = Nothing }

killQual :: Con.Expr -> Con.Expr
killQual = everywhere (mkT unQual) where
  unQual :: Con.QName -> Con.QName
  unQual (Con.Qual _ x) = unQual x
  unQual x = x

tcAndLoadPublicNames :: FilePath -> String -> TCMT IO (Map Text Text)
tcAndLoadPublicNames path basepn = do
  source <- parseSource . SourceFile =<< liftIO (absolute path)
  cr <- typeCheckMain TypeCheck source

  let iface = crInterface cr

  setScope (iInsideScope iface)
  scope <- getScope

  li <- fmap catMaybes . for (toList (_scopeInScope scope)) $ \name -> do
    t <- getConstInfo' name
    case t of
      Left _ -> pure Nothing
      Right d -> do
        expr <- reify . killDomainNames $ defType d
        t <- fmap (render . pretty . killQual) .
          abstractToConcrete_ . removeImpls $ expr

        case rangeFile (nameBindingSite (qnameName name)) of
          S.Just (filePath -> f)
            | ("Agda/Builtin" `isInfixOf` f) || ("Agda/Primitive" `isInfixOf` f) ->
              pure $ do
                fp <- fakePath name
                pure (name, fp, t)
            | otherwise -> do
              let
                f' = moduleName $ dropExtensions (makeRelative basepn f)
                modMatches = f' `isPrefixOf` render (pretty name)

              pure $ do
                unless modMatches Nothing
                pure (name, f' <.> "html", t)
          S.Nothing -> pure Nothing

  let
    f (name, modn, ty) =
      case rStart (nameBindingSite (qnameName name)) of
        Just pn -> pure (Text.pack (modn <> "#" <> show (posPos pn)), Text.pack ty)
        Nothing -> Nothing

  pure (Map.fromList (mapMaybe f li))

fakePath :: QName -> Maybe FilePath
fakePath (QName (MName xs) _) =
  listToMaybe
    [ l <.> "html"
    | l <- map (intercalate ".") (inits (map (render . pretty . nameConcrete) xs))
    , l `elem` builtinModules
    ]

removeImpls :: Expr -> Expr
removeImpls (Pi _ (x :| xs) e) =
  makePi (map (mapExpr removeImpls) $ filter ((/= Hidden) . getHiding) (x:xs)) (removeImpls e)
removeImpls (Fun span arg ret) =
  Fun span (removeImpls <$> arg) (removeImpls ret)
removeImpls e = e

makePi :: [TypedBinding] -> Expr -> Expr
makePi [] = id
makePi (b:bs) = Pi exprNoRange (b :| bs)

builtinModules :: [String]
builtinModules =
  [ "Agda.Builtin.Bool"
  , "Agda.Builtin.Char"
  , "Agda.Builtin.Cubical.HCompU"
  , "Agda.Builtin.Cubical.Path"
  , "Agda.Builtin.Cubical.Sub"
  , "Agda.Builtin.Float"
  , "Agda.Builtin.FromNat"
  , "Agda.Builtin.FromNeg"
  , "Agda.Builtin.Int"
  , "Agda.Builtin.List"
  , "Agda.Builtin.Maybe"
  , "Agda.Builtin.Nat"
  , "Agda.Builtin.Reflection"
  , "Agda.Builtin.Sigma"
  , "Agda.Builtin.String"
  , "Agda.Builtin.Unit"
  , "Agda.Builtin.Word"
  , "Agda.Primitive.Cubical"
  , "Agda.Primitive"
  ]
