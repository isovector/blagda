{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import           Blagda
import           Blagda.Diagrams
import           Blagda.Markdown
import           Blagda.Utils
import           Control.Monad.IO.Class
import           Control.Monad.Writer
import           Data.Foldable
import           Data.List
import qualified Data.Map as M
import           Data.Maybe (catMaybes)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Development.Shake
import           Development.Shake.FilePath
import           Development.Shake.Forward (shakeArgsForward, forwardOptions, cacheAction)
import qualified System.Directory as Dir
import           Text.DocTemplates (Context(..), toVal)
import           Text.HTML.TagSoup
import           Text.Pandoc (Pandoc(Pandoc))

main :: IO ()
main =
  shakeArgsForward
    (forwardOptions $ shakeOptions
      { shakeFiles="_build"
      , shakeLintInside=["site"]
      , shakeChange=ChangeDigest
      , shakeVersion = "8"
      }) $ do


  agda_files <- agdaHTML
  fileIdents <- liftIO $ newCacheIO parseFileIdents
  fileTypes <- liftIO $ newCacheIO parseFileTypes

  md_files' <- getDirectoryFiles "site" ["**/*.md"]
  let md_files = Set.toList $ Set.fromList md_files' Set.\\ Set.fromList agda_files

  commit <- gitCommit

  md0   <- sort . fmap ("_build/html0" </>) <$> getDirectoryFiles "_build/html0" ["*.md"]
  html0 <- sort . fmap ("_build/html0" </>) <$> getDirectoryFiles "_build/html0" ["*.html"]

  let getHtml1Path = getBuildPath "html1" "html"
      getHtmlPath = getBuildPath "html" "html"


  articles <-
    forP (fmap ("site" </>) md_files <> md0) $ \input -> do
      md@(Pandoc meta _) <- loadMarkdown commit input
      writeTemplate "support/web/template.html" mempty fileIdents md $ getHtml1Path input
      pure $ parseHeader (Text.pack $ dropExtension $ takeFileName input) meta

  let posts = reverse $ sortOn a_datetime $ catMaybes articles

  writeTemplate "support/web/index.html" (Context $ M.singleton "posts" $ toVal posts) fileIdents mempty $
    getHtml1Path "index.html"

  buildDiagrams

  void $ forP html0 $ \html ->
    liftIO $ Dir.copyFile html $ getHtml1Path html

  html1 <- sort . fmap ("_build/html1" </>) <$> getDirectoryFiles "_build/html1" ["*.html"]

  void $ forP html1 $ \input -> do
    let out = getHtmlPath input
    text <- liftIO $ Text.readFile input
    tags <- traverse (addLinkType fileIdents fileTypes) $ parseTags text
    traverse_ (checkMarkup (takeFileName out)) tags
    writeFile' out $ Text.unpack $  renderHTML5 tags

  sass <- getDirectoryFiles "" ["support/web/*.scss"]
  void $ forP sass $ \input ->
    cacheAction input $
      command_ [] "sass" [input, getBuildPath "html/css" "css" input]

  statics <- getDirectoryFiles "support/static" ["**/*"]
  void $ forP statics $ \filepath ->
    copyFileChanged ("support/static" </> filepath) ("_build/html" </> filepath)


checkMarkup :: FilePath -> Tag Text -> Action ()
checkMarkup file (TagText txt)
  |  "<!--" `Text.isInfixOf` txt || "<!–" `Text.isInfixOf` txt
  || "-->" `Text.isInfixOf` txt  || "–>" `Text.isInfixOf` txt
  = fail $ "[WARN] " ++ file ++ " contains misplaced <!-- or -->"
checkMarkup _ _ = pure ()



gitCommit :: Action String
gitCommit = do
  Stdout t <- command [] "git" ["rev-parse", "--verify", "HEAD"]
  pure (head (lines t))

