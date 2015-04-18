--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid            (mappend)
import           Hakyll
import           System.FilePath.Posix  (takeBaseName, takeDirectory
                                        , (</>), replaceDirectory )

--------------------------------------------------------------------------------

-- replace a foo/bar.md by foo/bar/index.html
-- this way the url looks like: foo/bar in most browsers
niceRoute :: Routes
niceRoute = customRoute createIndexRoute
  where
    createIndexRoute ident =
        takeDirectory (toFilePath ident) </> takeBaseName (toFilePath ident) </> "index.html"

baseRoute :: Routes
baseRoute = customRoute base
  where
    base ident = replaceDirectory (toFilePath ident) ""

baseRouteHTML :: Routes
baseRouteHTML = customRoute base
  where
    base ident = takeDirectory (replaceDirectory (toFilePath ident) "") </> takeBaseName (toFilePath ident) </> "index.html"



main :: IO ()
main = hakyll $ do

    match "static/favicon.ico" $ do
        route   baseRoute
        compile copyFileCompiler

    match "static/img/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "pages/about.markdown" $ do
        route   $ baseRouteHTML 
        let aboutCtx =
                constField "nav-selection-about" "true"        `mappend`
                defaultContext
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/base.html" aboutCtx
            >>= relativizeUrls

    match "pages/styleguide.markdown" $ do
        route   $ baseRouteHTML 
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/base.html" defaultContext
            >>= relativizeUrls

    match "thoughts/draft/*" $ do
        route $ niceRoute
        let draftCtx =
                constField "draft"   "true"     `mappend`
                defaultContext
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" draftCtx
            >>= loadAndApplyTemplate "templates/base.html" draftCtx
            >>= relativizeUrls

    match "thoughts/*" $ do
        route $ niceRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/base.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route niceRoute
        compile $ do
            posts <- recentFirst =<< loadAll "thoughts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archive"            `mappend`
                    constField "nav-selection-archive" "true"        `mappend`
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive-post-list.html" archiveCtx
                >>= loadAndApplyTemplate "templates/base.html" archiveCtx
                >>= relativizeUrls

    create ["index.html"] $ do
        route idRoute
        compile $ do
            posts <-  fmap (take 5) . recentFirst =<<  loadAllSnapshots "thoughts/*" "content"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                         `mappend`
                    constField "nav-selection-thoughts" "true"        `mappend`
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/post-list.html" indexCtx
                >>= loadAndApplyTemplate "templates/base.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
