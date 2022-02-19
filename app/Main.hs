{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import           Blagda.Agda
import           Blagda.Markdown
import           Control.Monad.IO.Class
import           Control.Monad.Writer
import           Data.Foldable
import           Data.List
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Development.Shake
import           Development.Shake.FilePath
import           Development.Shake.Forward (shakeArgsForward, forwardOptions, cacheAction)
import qualified System.Directory as Dir
import           Text.HTML.TagSoup
import Blagda



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


  agda_files <- agdaHTML

  md_files' <- getDirectoryFiles "site" ["**/*.md"]
  let md_files = Set.toList $ Set.fromList md_files' Set.\\ Set.fromList agda_files

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
    writeFile' out $ Text.unpack $  renderHTML5 tags

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

