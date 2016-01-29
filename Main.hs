--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import           Data.List              (isInfixOf)
import           Data.Monoid            ((<>))
import           Hakyll hiding (withUrls)
import qualified Text.HTML.TagSoup               as TS
import           System.FilePath.Posix  (takeBaseName,takeDirectory
                                         ,(</>),splitFileName, replaceDirectory )

--------------------------------------------------------------------------------
-- mconcat, mappend
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


niceBaseRoute :: Routes
niceBaseRoute = customRoute base
  where
    base ident = takeBaseName (toFilePath ident) </> "index.html"



-- replace url of the form foo/bar/index.html by foo/bar
removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml item = return $ fmap (withUrls removeIndexStr) item
  where
    removeIndexStr :: String -> String
    removeIndexStr url = case splitFileName url of
        (dir, "index.html") | isLocal dir -> dir
        _                                 -> url
    isLocal :: String -> Bool
    isLocal uri = not ("://" `isInfixOf` uri)




--------------------------------------------------------------------------------
-- | Apply a function to each URL on a webpage
withUrls :: (String -> String) -> String -> String
withUrls f = withTags tag
  where
    tag (TS.TagOpen s a) = TS.TagOpen s $ map attr a
    tag x                = x
    attr (k, v)          = (k, if isUrlAttribute k then f v else v)

isUrlAttribute :: String -> Bool
isUrlAttribute = (`elem` ["src", "href", "data", "data-url"])





feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle       = "Mechanical Elephant: Code, Design, Science"
    , feedDescription = "Haskell, Python, and Design Process"
    , feedAuthorName  = "Matthew Griffith"
    , feedAuthorEmail = "matt@mechanical-elephant.com"
    , feedRoot        = "http://mechanical-elephant.com"
    }

mainDescription :: String
mainDescription = "Programming in haskell, creativity, and the design process."

main :: IO ()
main = hakyll $ do

    match "static/favicon.ico" $ do
        route   baseRoute
        compile copyFileCompiler

    match "static/img/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "static/js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "static/js/vendor/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "pages/styleguide.markdown" $ do
        route niceBaseRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/alt-base.html" defaultContext
            >>= relativizeUrls
            >>= removeIndexHtml


    -- match "thoughts/book/*" $ do
    --     route niceRoute
    --     let draftCtx = constField "book"   "true"
    --                 <> defaultContext
    --     compile $ pandocCompiler
    --         >>= loadAndApplyTemplate "templates/post.html" draftCtx
    --         >>= loadAndApplyTemplate "templates/base.html" draftCtx
    --         >>= relativizeUrls
    --         >>= removeIndexHtml

    -- match "thoughts/draft/*" $ do
    --     route niceRoute
    --     let draftCtx = constField "draft"   "true"
    --                 <> defaultContext
    --     compile $ pandocCompiler
    --         >>= loadAndApplyTemplate "templates/post.html" draftCtx
    --         >>= loadAndApplyTemplate "templates/base.html" draftCtx
    --         >>= relativizeUrls
    --         >>= removeIndexHtml

    match "thoughts/*" $ do
        route niceRoute
        let thoughtCtx = constField "nav"   "true"
                      <> postCtx
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html" thoughtCtx
            >>= saveSnapshot "rendered"
            >>= loadAndApplyTemplate "templates/alt-base.html" thoughtCtx
            >>= relativizeUrls
            >>= removeIndexHtml


    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx <> bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "thoughts/*" "rendered"
            renderAtom feedConfig feedCtx posts

    create ["rss"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx <> bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "thoughts/*" "rendered"
            renderRss feedConfig feedCtx posts

    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            pages <- loadAll ("pages/about.markdown" .||. "thoughts/*")
            let sitemapCtx = listField "pages" sitemapItemCtx (return pages)
                          <> defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
                >>= removeIndexHtml
                >>= relativizeUrls



    match "pages/about.markdown" $ do
        let aboutCtx = constField "title" "About Mechanical Elephant"
                    <> constField "nav-selection-about" "true"
                    <> constField "description" mainDescription
                    <> defaultContext
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/pass-through.html" aboutCtx
            -- >>= relativizeUrls


    create ["low-poly.html"] $ do
        route niceRoute
        compile $ do
            makeItem ""
                >>= loadAndApplyTemplate "templates/low-poly.html" defaultContext
                -- >>= removeIndexHtml
                -- >>= relativizeUrls



    create ["index.html"] $ do
        route idRoute
        compile $ do
            posts <- chronological =<< loadAll "thoughts/*"
            aboutBody <- loadBody "pages/about.markdown" :: Compiler String
            let indexCtx = listField "articles" postCtx (return posts)
                        <> constField "title" "Mechanical Elephant"
                        <> constField "nav-selection-thoughts" "true"
                        <> constField "description" mainDescription
                        <> constField "about-body" aboutBody
                        <> defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/project-list.html" indexCtx
                >>= loadAndApplyTemplate "templates/alt-base.html" indexCtx
                >>= relativizeUrls
                >>= removeIndexHtml

    match "templates/*" $ compile templateCompiler



--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext

teaserCtx :: Context String
teaserCtx = teaserField "teaser" "content" <> postCtx


sitemapItemCtx :: Context String
sitemapItemCtx = mconcat [ constField "root" (feedRoot feedConfig)
                         , dateField "isodate" "%F"
                         , defaultContext
                         ]
