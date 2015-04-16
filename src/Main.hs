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

    match "static/css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "pages/about.rst" $ do
        route   $ baseRouteHTML 
        let aboutCtx =
                    constField "nav-selection-about" "true"        `mappend`
                    defaultContext
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/base.html" aboutCtx
            >>= relativizeUrls

    match "posts/*" $ do
        route $ niceRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/base.html" postCtx
            >>= relativizeUrls

    match "posts/*" $ version "brief" $ do
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route niceRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
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
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasVersion "brief")
            let indexCtx =
                    listField "posts" postCtx (return (take 5 posts)) `mappend`
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
