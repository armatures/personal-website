--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid           (mappend)
import           Hakyll

import qualified Data.ByteString.Char8 as Char8
import           Data.ByteString.Lazy  (ByteString, unpack)
import qualified Data.Text             as T
import           Debug.Trace           as Tr
import           PandocFilterGraphviz

import           Text.Pandoc           (Format (..), Pandoc, ReaderOptions,
                                        WriterOptions (..), runIO, writeLaTeX)
import           Text.Pandoc.PDF       (makePDF)
import           Text.Pandoc.Walk      (walk, walkM)

import           Hakyll.Web.Html       (withUrls)

--------------------------------------------------------------------------------
main :: IO ()
main =
  hakyll $ do
    match "static/*" $ do
      route idRoute
      compile copyFileCompiler
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler
    match "mscgen-images/*.svg" $ do
      route idRoute
      compile copyFileCompiler
    match "graphviz-images/*.svg" $ do
      route idRoute
      compile copyFileCompiler
    match "css/*" $ do
      route idRoute
      compile compressCssCompiler
    match "posts/*" $ do
      version "pdf" $ do
        route $ setExtension "pdf"
        -- todo find a way we can pass the context here
        compile $ do
          body@(Item id bod) <- getResourceBody
          readPandocWith defaultHakyllReaderOptions body >>=
            relativizeUrlsWithCompiler "." >>=
            traverse
              (unsafeCompiler .
               walkM
                 (renderAll $
                  RenderAllOptions {urlPrefix = Just "./", renderFormat = EPS})) >>=
            writePandocLatexWith body
      version "full" $ do
        route $ setExtension "html"
        compile $
          customPostPandocCompiler >>=
          loadAndApplyTemplate "templates/post.html" postCtx >>=
          loadAndApplyTemplate "templates/default.html" postCtx >>=
          saveSnapshot "content"
      version "teaser" $ do
        route $ setExtension "toc-html"
        compile $ customTeaserPandocCompiler >>= saveSnapshot "content"
    match "index.html" $ do
      route idRoute
      compile $ do
        posts <-
          recentFirst =<<
          loadAllSnapshots ("posts/*" .&&. hasVersion "teaser") "content"
        let indexCtx =
              listField "posts" teaserCtx (return posts) `mappend`
              constField "title" "Home" `mappend`
              pageDefaultContext
        getResourceBody >>= applyAsTemplate indexCtx >>=
          loadAndApplyTemplate "templates/default.html" indexCtx >>=
          replaceTocExtension >>=
          relativizeUrls
    match "templates/*" $ compile templateBodyCompiler
    match "robots.txt" $ do
      route idRoute
      compile $ getResourceBody >>= relativizeUrls
    create ["sitemap.xml"] $ do
      route idRoute
      compile $ do
        posts <-
          recentFirst =<<
          loadAllSnapshots ("posts/*" .&&. hasVersion "full") "content"
        pages <- loadAll "pages/*"
        let allPosts = return (pages ++ posts)
        let sitemapCtx =
              mconcat [listField "entries" postCtx allPosts, pageDefaultContext]
        makeItem "" >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx `mappend` bodyField "description"
                  -- constField "description" "This is the post description"
        posts <-
          fmap (take 10) . recentFirst =<<
          loadAllSnapshots ("posts/*" .&&. hasVersion "full") "content"
        renderAtom feedConfiguration feedCtx posts

--------------------------------------------------------------------------------
baseUrl :: String
baseUrl = "https://www.justus.pw"

pageTitle :: String
pageTitle = "Justus Perlwitz"

authorName :: String
authorName = "Justus Perlwitz"

pageDefaultContext :: Context String
pageDefaultContext =
  constField "baseUrl" baseUrl `mappend` constField "pageTitle" pageTitle `mappend`
  constField "authorName" authorName `mappend`
  defaultContext

postCtx :: Context String
postCtx =
  dateField "lastmod" "%Y-%m-%d" `mappend` dateField "date" "%B %e, %Y" `mappend`
  pageDefaultContext

teaserCtx :: Context String
teaserCtx = teaserField "teaser" "content" `mappend` postCtx

--- For RSS
feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
    { feedTitle = pageTitle
    , feedDescription = "Articles about software and life"
    , feedAuthorName = authorName
    , feedAuthorEmail = "hello@justus.pw"
    , feedRoot = baseUrl
    }

---
postHakyllWriterOptions :: WriterOptions
postHakyllWriterOptions =
  defaultHakyllWriterOptions
    { writerSectionDivs = True
    , writerTableOfContents = True
    , writerColumns = 120
    , writerTemplate =
        Just
          "<div id=\"TOC\">$toc$</div>\n<div id=\"markdownBody\">$body$</div>"
    , writerTOCDepth = 4
    , writerHtmlQTags = True
    }

customPostPandocCompiler :: Compiler (Item String)
customPostPandocCompiler =
  pandocCompilerWithTransformM
    defaultHakyllReaderOptions
    postHakyllWriterOptions
    (unsafeCompiler .
     walkM
       (renderAll $ RenderAllOptions {urlPrefix = Nothing, renderFormat = SVG}))

customTeaserPandocCompiler :: Compiler (Item String)
customTeaserPandocCompiler =
  pandocCompilerWithTransform
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
    (walk stripHeading)

-- Some code copy for pdf creation, taken from Hakyll.Web.Pandoc
writePandocLatexWith :: Item String -> Item Pandoc -> Compiler (Item ByteString)
writePandocLatexWith item (Item itemi doc) = do
  author <- getStringFromContext "authorName"
  title <- getStringFromContext "title"
  date <- getStringFromContext "lastmod"
  let variables = [("author", author), ("title", title), ("date", date)]
  pdfString <-
    unsafeCompiler $ do
      template <- readFile "templates/post.tex"
      let options = pdfHakyllWriterOptions variables template
      pdf <- runIO $ makePDF "xelatex" [] writeLaTeX options doc
      case pdf of
        Left err -> error $ "Main.writePandocLatexWith:" ++ show err
        Right result ->
          case result of
            Left err     -> error $ "Main.writePandocLatexWith:" ++ show err
            Right result -> return result
  makeItem pdfString
  where
    getString (StringField s) = return s
    getStringFromContext s = unContext postCtx s [] item >>= getString

relativizeUrlsWithCompiler :: String -> Item Pandoc -> Compiler (Item Pandoc)
relativizeUrlsWithCompiler root item =
  return $ fmap (walk $ relativizePandocUrls ".") item

pdfHakyllWriterOptions :: [(String, String)] -> String -> WriterOptions
pdfHakyllWriterOptions options template =
  defaultHakyllWriterOptions
    { writerSectionDivs = True
    , writerTableOfContents = True
    , writerTOCDepth = 4
    , writerTemplate = Just template
    , writerVariables = latexVariables
    }
  where
    latexVariables :: [(String, String)]
    latexVariables =
      options ++
      [ ("geometry", "margin=2cm")
      , ("CJKmainfont", "Kozuka Mincho Pro")
      , ("monofont", "Courier New")
      ]

-- TOC creation
replaceTocExtension :: Item String -> Compiler (Item String)
replaceTocExtension = return . fmap (withUrls replacer)
  where
    replacer = replaceAll "\\.toc-html" (const ".html")
