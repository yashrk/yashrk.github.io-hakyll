{-# LANGUAGE OverloadedStrings #-}

import Hakyll
import Text.Pandoc.Options

compileTemplates :: Rules ()
compileTemplates = match "templates/*.html" $ compile templateCompiler

myWriterOptions :: WriterOptions
myWriterOptions = defaultHakyllWriterOptions
    { writerSectionDivs = True
    }

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
    compile $ (pandocCompilerWith defaultHakyllReaderOptions myWriterOptions)
      >>= loadAndApplyTemplate "templates/webmention-comment.html" defaultContext

  match "*-en.md" $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default-en.html" defaultContext

  match ("**-ru.md" .||. "index.md") $ do
    route   $ setExtension "html"
    compile $ (pandocCompilerWith defaultHakyllReaderOptions myWriterOptions)
      >>= loadAndApplyTemplate "templates/default-ru.html" defaultContext
