# Sources for yashrk.github.io

Source code to generate yashrk.github.io pages with Hakyll (https://jaspervdj.be/hakyll/) static site generator.

## How to compile

```bash
cabal run site build
```

## How to run continuous build and preview

```bash
cabal run site watch
```

## What to do if `site.hs` was edited

```bash
cabal run site rebuild
```

## How to delete work files

```bash
cabal run site clean
```