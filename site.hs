{-# LANGUAGE OverloadedStrings #-}

import Hakyll
import Text.Pandoc.Options
import Data.List
import Data.List.Split

-- Utilities

compileTemplates :: Rules ()
compileTemplates = match "templates/*.html" $ compile templateCompiler

-- Configuration

myWriterOptions :: WriterOptions
myWriterOptions = defaultHakyllWriterOptions
    { writerSectionDivs = True
    }

feedConfiguration :: FeedConfiguration
feedConfiguration =  FeedConfiguration
    { feedTitle       = "Юрий Широков"
    , feedDescription = "yashrk's notes"
    , feedAuthorName  = "Юрий Широков"
    , feedAuthorEmail = "yuriy.shirokov@gmail.com"
    , feedRoot        = "https://yashrk.github.io"
    }

-- Blog feed

postsSnapshot :: Snapshot
postsSnapshot = "posts"

normalDateCtx :: Context String
normalDateCtx = dateField "date" "%d.%m.%0Y"
  <> defaultContext

allPostsContext :: Context String
allPostsContext = listField "postList" normalDateCtx loadPosts
  <> normalDateCtx

loadPosts :: Compiler [Item String]
loadPosts = loadAllSnapshots "posts/*-ru.md" postsSnapshot >>= recentFirst

-- General site description

main :: IO ()
main = hakyll $ do
  compileTemplates

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "fonts/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "images/**" $ do
    route   idRoute
    compile copyFileCompiler

  match "files/**" $ do
    route   idRoute
    compile copyFileCompiler

  match "*.html" $ do
    route   idRoute
    compile copyFileCompiler

  match "*.xml" $ do
    route   idRoute
    compile copyFileCompiler

  match "webmentions/comments/*.md" $ do
    route   $ setExtension "html"
    compile $ pandocCompilerWith defaultHakyllReaderOptions myWriterOptions
      >>= loadAndApplyTemplate "templates/webmention-comment.html" defaultContext

  match "webmentions/likes/*.md" $ do
    route   $ setExtension "html"
    compile $ pandocCompilerWith defaultHakyllReaderOptions myWriterOptions
      >>= loadAndApplyTemplate "templates/webmention-like.html" defaultContext

  match "*-en.md" $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default-en.html" defaultContext

  match ("*-ru.md" .||. "travels/*.md" .||. "index.md") $ do
    route   $ setExtension "html"
    compile $ do
      pandocCompilerWith defaultHakyllReaderOptions myWriterOptions
      >>= loadAndApplyTemplate "templates/default-ru.html" defaultContext

  match ("posts/*-ru.md") $ do
    route   $ setExtension "html"
    compile $ do
      pandocCompilerWith defaultHakyllReaderOptions myWriterOptions
      >>= saveSnapshot postsSnapshot
      >>= loadAndApplyTemplate "templates/post-ru.html" allPostsContext

  match ("blog/blog-ru.md") $ do
    route   $ constRoute "blog-ru.html"
    compile $ do
      pandocCompilerWith defaultHakyllReaderOptions myWriterOptions
      >>= loadAndApplyTemplate "templates/blog-ru.html" allPostsContext

  create ["atom.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = allPostsContext `mappend` bodyField "description"
      posts <- fmap (take 20) . recentFirst =<< loadPosts
      renderAtom feedConfiguration feedCtx posts

  create ["rss.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = allPostsContext `mappend` bodyField "description"
      posts <- fmap (take 20) . recentFirst =<< loadPosts
      renderRss feedConfiguration feedCtx posts

