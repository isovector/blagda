{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import           Blagda
import           Blagda.Diagrams
import           Blagda.Markdown
import           Blagda.Utils
import           Control.Monad.IO.Class
import           Control.Monad.Writer
import           Data.Aeson
import           Data.Foldable
import           Data.List
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import           Development.Shake
import           Development.Shake.FilePath
import           Development.Shake.Forward (shakeArgsForward, forwardOptions)
import           GHC.Generics (Generic)
import qualified System.Directory as Dir
import           Text.HTML.TagSoup
import           Text.Pandoc (Meta (Meta))
import Debug.Trace (trace)

parseHeader :: Meta -> Maybe Article
parseHeader (Meta m) =
  Article
    <$> (parseMetaString =<< Map.lookup "title" m)
    <*> ( parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M"
          . Text.unpack =<< parseMetaString
                        =<< Map.lookup "date" m)

data Article = Article
  { a_title    :: Text
  , a_datetime :: UTCTime
  }
  deriving (Eq, Ord, Show, Generic, ToJSON)


main :: IO ()
main =
  shakeArgsForward
    (forwardOptions $ shakeOptions
      { shakeFiles="_build"
      , shakeLintInside=["site"]
      , shakeChange=ChangeDigest
      , shakeVersion = "15"
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


  raw_articles <-
    forP (fmap ("site" </>) md_files) $ \input ->
      loadMarkdown parseHeader commit input
  raw_md <-
    forP md0 $ fmap (\s -> s { p_path = dropDirectory1 $ p_path s } ) . loadMarkdown parseHeader commit

  void $ forP html0 $ \html ->
    liftIO $ Dir.copyFile html $ getHtml1Path html

  let renamed_articles = rename doMyRename $ raw_articles <> raw_md
  liftIO $ traverse_ (putStrLn . p_path) renamed_articles

  articles <- forP renamed_articles $ renderPost fileIdents defaultWriterOptions


  writeTemplate "template.html" articles

  let posts = reverse $ sortOn (a_datetime . p_meta) $ catMaybes $ fmap sequenceA articles
  writeTemplate "index.html" $ pure $ Post "index.html" mempty $ toJSON posts

  liftIO $ Dir.createDirectoryIfMissing True "_build/html"
  buildDiagrams

  html1 <- getDirectoryFiles "_build/html1" ["**/*.html"]

  void $ forP html1 $ \input -> do
    let out = "_build/html" </> input
    text <- liftIO $ Text.readFile $ "_build/html1" </> input
    tags <- traverse (addLinkType fileIdents fileTypes) $ parseTags text
    writeFile' out $ Text.unpack $ renderHTML5 tags

  sass <- getDirectoryFiles "" ["support/web/*.scss"]
  void $ forP sass $ \input ->
    command_ [] "sass" [input, getBuildPath "html/css" "css" input]

  statics <- getDirectoryFiles "support/static" ["**/*"]
  void $ forP statics $ \filepath ->
    copyFileChanged ("support/static" </> filepath) ("_build/html" </> filepath)

traceOf :: Show b => (a -> b) -> a -> a
traceOf f a = trace (show $ f a) a


doMyRename :: FilePath -> FilePath
doMyRename s
  | isPrefixOf "Blog/20" s = "blog" </> drop (length @[] "Blog/2000-00-00-") s
  | isPrefixOf "Blog" s = "blog" </> drop 5 s
  | isPrefixOf "html0/Blog" s = "blog" </> drop (length @[] "html0/Blog.") s
  | otherwise = s


gitCommit :: Action String
gitCommit = do
  Stdout t <- command [] "git" ["rev-parse", "--verify", "HEAD"]
  pure (head (lines t))

