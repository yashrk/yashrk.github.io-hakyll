{-# LANGUAGE OverloadedStrings #-}

import Hakyll

compileTemplates :: Rules ()
compileTemplates = match "templates/*.html" $ compile templateCompiler

main :: IO ()
main = hakyll $ do
  compileTemplates

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "fonts/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "*.html" $ do
    route   idRoute
    compile copyFileCompiler

  match "*-en.md" $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default-en.html" defaultContext

  match ("*-ru.md" .||. "index.md") $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default-ru.html" defaultContext
