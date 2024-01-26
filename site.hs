{-# LANGUAGE OverloadedStrings #-}

import Hakyll
import Text.Pandoc.Options

-- Utilities

compileTemplates :: Rules ()
compileTemplates = do
  match ("templates/*.html") $ compile templateCompiler
  match ("templates/*.xml") $ compile templateCompiler

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
  <> constField "root" root                                   -- needed for sitemap.xml
  <> normalDateCtx

loadPosts :: Compiler [Item String]
loadPosts = loadAllSnapshots "posts/*-ru.md" postsSnapshot >>= recentFirst

loadPostsByPattern :: Pattern -> Compiler [Item String]
loadPostsByPattern pattern = loadAllSnapshots pattern postsSnapshot >>= recentFirst

addTagsToAllPostsContext :: Tags -> Context String
addTagsToAllPostsContext tags = tagsField "tags" tags <> allPostsContext

-- sitemap

root :: String
root = "https://yashrk.github.io"

-- General site description

main :: IO ()
main = hakyll $ do
  -- Templates
  compileTemplates

  -- Tags
  tags <- buildTags "posts/*" (fromCapture "tags/*.html")

  --
  -- Rules
  --

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
      >>= loadAndApplyTemplate "templates/post-ru.html" (addTagsToAllPostsContext tags)

  match ("blog/blog-ru.md") $ do
    route   $ constRoute "blog-ru.html"
    compile $ do
      pandocCompilerWith defaultHakyllReaderOptions myWriterOptions
      >>= loadAndApplyTemplate "templates/blog-ru.html" allPostsContext

  tagsRules tags $ \tag pattern -> do
        let title = "Посты по тегу «" ++ tag ++ "»"
        let tldr = "Посты по тегу \"" ++ tag ++ "\""
        let snippetImage = "snippetImage: /images/capybara.jpeg"
        let defaultDate = "2024-01-25"
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadPostsByPattern pattern
            let ctx = constField "title" title
                      <> constField "tldr" tldr
                      <> constField "snippetImage" snippetImage
                      <> constField "published" defaultDate
                      <> listField "postList" (addTagsToAllPostsContext tags) (return posts)
                      <> defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts-by-tag.html" ctx

  create ["atom.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = allPostsContext <> bodyField "description"
      posts <- fmap (take 20) . recentFirst =<< loadPosts
      renderAtom feedConfiguration feedCtx posts

  create ["rss.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = allPostsContext <> bodyField "description"
      posts <- (take 20) <$> (recentFirst =<< loadPosts)
      renderRss feedConfiguration feedCtx posts

  create ["sitemap.xml"] $ do
    route idRoute
    compile $ do
      posts <- loadPosts
      uniquePages <- loadAll (fromList ["blog/blog-ru.md"])
      ruPages <- loadAll ("*-ru.md" .||. "travels/*.md" .||. "index.md")
      enPages <- loadAll "*-en.md"
      let pages = posts <> uniquePages <> ruPages <> enPages
      let sitemapCtx =
            dateField "date" "%0Y-%m-%d"
            <> constField "root" root
            <> listField "pages" allPostsContext (return pages)
      makeItem ""
        >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
