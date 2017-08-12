{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.Doc.Writer.Html
-- Copyright   :  2015-2017 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
--
-- Maintainer  :  mschenner.dev@gmail.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- HTML writer: Convert Doc to HTML.
----------------------------------------------------------------------

module Text.Doc.Writer.Html
 ( -- * Conversion to HTML
   -- ** Documents
   doc2html
 , doc2multiHtml
 , mdoc2htmlPages
 , mdoc2epubPages
   -- ** Document elements
 , sections2html
 , blocks2html
 , inlines2html
   -- ** Special pages
 , htmlTitlePage
 , htmlNavPage
 ) where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative
#endif
import Control.Monad.Trans.Reader (Reader, runReader, asks)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text.Lazy (Text)
import qualified Data.Text as T
import Text.Blaze ((!), (!?), string, text, stringValue, textValue)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as A
import qualified Text.Blaze.XHtml1.Strict as H1
import qualified Text.Blaze.XHtml1.Strict.Attributes as A1

import Text.Bib.Writer
import Text.Doc.Filter.DeriveSection
import Text.Doc.Filter.MultiFile
import Text.Doc.Section
import Text.Doc.Types
import Text.Doc.Writer.Core


---------- Doc conversion

-- | Convert a 'Doc' document to HTML.
doc2html :: Doc -> Text
doc2html = renderHtml . convertDoc . prepDoc

-- Prepare a raw 'Doc' document for HTML conversion functions:
-- Convert to section view, add bibliography and notes sections.
prepDoc :: Doc -> SectionDoc
prepDoc = addBibliography . addNotes . doc2secdoc

-- Convert a 'SectionDoc' to HTML Markup.
convertDoc :: SectionDoc -> Html
convertDoc doc =
  let renderDoc = wrapPage (mkBody doc)
  in runReader renderDoc (docMeta doc)


---------- MultiDoc conversion

-- | Convert a 'Doc' document to a set of HTML pages.
--
-- This function provides a default way of splitting a
-- document into multiple files at the chapter level.
-- Each page includes a document header (see 'mdoc2htmlPages').
doc2multiHtml :: Doc -> [(FilePath, Text)]
doc2multiHtml = M.assocs . mdoc2htmlPages . toMultiFileDoc 2 . prepDoc

-- | Convert a 'MultiFileDoc' document to a set of HTML pages.
--
-- Each page includes a document header.
mdoc2htmlPages :: MultiFileDoc -> Map FilePath Text
mdoc2htmlPages = mdoc2html standaloneWithHeader

-- | Convert a 'MultiFileDoc' document to a set of HTML pages.
--
-- The pages do not include a document header.
mdoc2epubPages :: MultiFileDoc -> Map FilePath Text
mdoc2epubPages = mdoc2html standalone

-- Convert a 'MultiFileDoc' document to a set of HTML pages,
-- where each page is wrapped with the provided combinator.
mdoc2html :: (Reader Meta Html -> Reader Meta Html) -> MultiFileDoc -> Map FilePath Text
mdoc2html pageWrapper (MultiFileDoc meta _ fileMap) =
  let convertPage (ContentFile sec) =
        convert2html (pageWrapper . section) meta sec
  in M.mapKeys filenameFromID $ M.map convertPage fileMap


---------- Sub-Doc conversion: render parts of documents

-- | Convert 'Section' elements to an HTML fragment.
sections2html :: Meta -> [Section] -> Text
sections2html = convert2html sections

-- | Convert 'Block' elements to an HTML fragment.
--
-- Note: This function does not convert blocks to sections
-- and thus will not create section elements for headers.
-- For a proper rendering of headers, use 'blocks2sections'
-- and 'sections2html' instead.
blocks2html :: Meta -> [Block] -> Text
blocks2html = convert2html blocks

-- | Convert 'Inline' elements to an HTML fragment.
inlines2html :: Meta -> [Inline] -> Text
inlines2html = convert2html inlines

-- Apply renderer to document content.
convert2html :: (a -> Reader Meta Html) -> Meta -> a -> Text
convert2html render meta docdata =
  renderHtml (runReader (render docdata) meta)


---------- Create special pages

-- | Create an HTML title page for a document.
--
-- The page body consists only of a @\<header\>@ element.
htmlTitlePage :: HasMeta d => d -> Text
htmlTitlePage doc = renderHtml $
  runReader (wrapPage (H.body <$> header)) (docMeta doc)

-- | Create an HTML table of contents page for a multi-file document.
--
-- The page body consists only of a @\<nav\>@ element. No @\<header\>@.
htmlNavPage :: MultiFileDoc -> Text
htmlNavPage (MultiFileDoc meta nav _) =
  convert2html (standalone . toc) meta nav


---------- Page level combinators

-- Wrap content in a standalone HTML page
-- (without a document header).
standalone :: Reader Meta Html -> Reader Meta Html
standalone content =
  wrapPage (H.body <$> wrapMain content)

-- Wrap content in a standalone HTML page
-- with a default document header.
standaloneWithHeader :: Reader Meta Html -> Reader Meta Html
standaloneWithHeader content =
  wrapPage (H.body <$> header <+> wrapMain content)

-- Wrap content with doctype, @<html>@ root element
-- and a default document @<head>@. No @<header>@.
wrapPage :: Reader Meta Html -> Reader Meta Html
wrapPage content = docTypeHtml <*> (mkHead <+> content)

-- Wrap content in a @<main>@ element.
wrapMain :: Reader Meta Html -> Reader Meta Html
wrapMain = (H.main `orDivClass` "main" <*>)


---------- meta

-- Create @<head>@ element.
mkHead :: Reader Meta Html
mkHead = do
  title <- asks metaTitle
  H.head <$>
    (elMetaCharset <+>
     (H.title <$> inlines title) <>$
     (H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0") <>$
     (H.meta ! A.name "generator" ! A.content "texhs"))

----- header

-- Create @<header>@ element.
header :: Reader Meta Html
header = do
  title <- asks metaTitle
  subtitle <- asks metaSubTitle
  authors <- asks metaAuthors
  H.header `orDivClass` "header" <*>
    ((heading 1 ! A.class_ "title" <$> inlines title) <+>
    unlessR (null subtitle)
      (heading 1 ! A.class_ "subtitle" <$> inlines subtitle) <+>
    (heading 2 ! A.class_ "author" <$> foldMapR inlines authors))

----- toc

-- Create a table of contents.
toc :: NavList -> Reader Meta Html
toc items =
  unlessR (null items)
    (H.nav `orDivClass` "nav" <!> A.id "toc" <*>
      (heading 2 "Contents" $<>
      (H.ul <$> foldMapR navitem items)))

-- Create a toc entry for a single section (and its subsections).
navitem :: NavListItem -> Reader Meta Html
navitem (NavListItem anchor title subitems) = do
  db <- asks metaAnchorFileMap
  H.li <$>
    ((H.a ! A.href (textValue (internalAnchorTarget db anchor)) <$>
      (sectionNumberPrefix anchor <+> inlines title)) <+>
    unlessR (null subitems)
      (H.ul <$> foldMapR navitem subitems))

-- Create a section number.
--
-- Includes a trailing space, ready to be prefixed to a section title.
-- For phantom (unnumbered) sections, the generated prefix is empty.
sectionNumberPrefix :: InternalAnchor -> Reader Meta Html
sectionNumberPrefix anchor@(SectionAnchor _) =
  unlessR (isPhantomSection anchor)
    (inlines (internalAnchorDescription anchor) <>$ text " ")
sectionNumberPrefix _ = memptyR


---------- content

-- Create @<body>@ element,
-- including @<header>@ and @<nav>@ (toc).
mkBody :: SectionDoc -> Reader Meta Html
mkBody (SectionDoc _ secs) = H.body <$>
  header <+>
  toc (mkNavList secs) <+>
  wrapMain (sections secs)

-- Convert 'Section' elements to HTML.
sections :: [Section] -> Reader Meta Html
sections = foldMapR section

-- Convert 'Block' elements to HTML.
blocks :: [Block] -> Reader Meta Html
blocks = foldMapR block

-- Convert 'Inline' elements to HTML.
inlines :: [Inline] -> Reader Meta Html
inlines = foldMapR inline

-- Convert a single 'Section' element to HTML.
section :: Section -> Reader Meta Html
section (Section hlevel hanchor htitle secbody subsecs) =
  elSection <!> A.id (textValue (internalAnchorID hanchor)) <*>
  ((heading hlevel <$>
      sectionNumberPrefix hanchor <+> inlines htitle) <+>
    blocks secbody <+>
    sections subsecs)

-- Convert a single 'Block' element to HTML.
--
-- Note: SectionDoc documents should not contain Header elements,
-- all header information is collected in Section elements.
block :: Block -> Reader Meta Html
block (Para xs) = H.p <$> inlines xs
block (Header level anchor htitle) =
  heading level ! A.id (textValue (internalAnchorID anchor)) <$>
  (sectionNumberPrefix anchor <+> inlines htitle)
block (List UnorderedList xss) =
  H.ul <$> foldMapR (fmap H.li . blocks) xss
block (List OrderedList xss) =
  H.ol <$> foldMapR (fmap H.li . blocks) xss
block (AnchorList ItemList xs) =
  H.ol ! A.class_ "numbered-item-list" <$>
  foldMapR itemlistitem xs
block (AnchorList NoteList xs) =
  H.ol ! A.class_ "notes" <$>
  foldMapR notelistitem xs
block (BibList citeEntries) =
  H.ol ! A.class_ "biblist" <$>
  foldMapR biblistitem citeEntries
block (QuotationBlock xs) = H.blockquote <$> blocks xs
block (Figure anchor mediaid imgdesc) = do
  imgloc <- asks (lookupMedia mediaid . metaMediaMap)
  H.figure `orDivClass` "figure" <!>
    A.id (textValue (internalAnchorID anchor)) <*>
      (H.img ! A.src (textValue imgloc) $<>
      (elFigcaption <*>
         ((text "Figure " $<>
           inlines (internalAnchorDescription anchor)) <+>
          (text ": " $<>
           inlines imgdesc))))
block (Table anchor tdesc tdata) =
  H.table ! A.id (textValue (internalAnchorID anchor)) <$>
    ((H.caption <$>
      ((text "Table " $<>
        inlines (internalAnchorDescription anchor)) <+>
       (text ": " $<>
        inlines tdesc))) <+>
     (H.tbody <$> foldMapR
       (fmap H.tr . foldMapR tableCell) tdata))
block (SimpleTable tdata) =
  H.table <$>
    (H.tbody <$> foldMapR
      (fmap H.tr . foldMapR tableCell) tdata)

-- Convert a single 'TableCell' element to HTML.
tableCell :: TableCell -> Reader Meta Html
tableCell (SingleCell xs) =
  H.td <$> inlines xs
tableCell (MultiCell i xs) =
  H.td ! A.colspan (stringValue (show i)) <$> inlines xs

-- Convert a single 'ItemList' item to HTML.
itemlistitem :: ListItem -> Reader Meta Html
itemlistitem (ListItem anchor xs) =
  H.li ! A.id (textValue (internalAnchorID anchor)) !
    A.class_ "numbered-item" !
    A.value (stringValue (show (internalAnchorLocalNum anchor))) <$>
    blocks xs

-- Convert a single 'NoteList' item (aka footnote) to HTML.
notelistitem :: ListItem -> Reader Meta Html
notelistitem (ListItem anchor fntext) =
  H.li ! A.id (textValue (internalAnchorID anchor)) <$>
    blocks (appendBackreference anchor fntext)

-- Append an anchor backreference to a list of blocks.
--
-- For example, insert a backreference into a footnote text
-- in order to refer back to the corresponding footnote mark.
appendBackreference :: InternalAnchor -> [Block] -> [Block]
appendBackreference anchor fntext =
  let target = internalAnchorSwitch anchor
      backrefText :: [Inline]
      backrefText = [Str "\x21A9"] -- LEFTWARDS ARROW WITH HOOK
      backref :: [Inline]
      backref = mkInternalLink backrefText target "" "note-backref"
  in case last fntext of
       Para xs -> init fntext ++ [Para (xs ++ (Space : backref))]
       _ -> fntext ++ [Para backref]

-- Create a single entry in the bibliography.
biblistitem :: CiteEntry -> Reader Meta Html
biblistitem (CiteEntry anchor _ _ formatted) =
  H.li ! A.id (textValue (internalAnchorID anchor)) <$>
  inlines formatted

-- Convert a single 'Inline' element to HTML.
inline :: Inline -> Reader Meta Html
inline (Str xs) = return $ string xs
inline (FontStyle s xs) = style s <$> inlines xs
inline (Math _ xs) =
  H.span ! A.class_ "math" <$>
  inlines xs
inline Space = return $ text " "
inline (Citation cit) = do
  db <- asks metaCiteDB
  H.span ! A.class_ "citation-group" <$>
    inlines (fmtMultiCite db cit)
inline (Pointer label protoAnchor) = do
  anchorDB <- asks metaAnchorMap
  fileDB <- asks metaAnchorFileMap
  let anchor = extractAnchor anchorDB label protoAnchor
  H.a ! A.href (textValue (anchorTarget fileDB anchor)) !?
    ( not (T.null (anchorType anchor))
    , A.class_ (textValue (anchorType anchor))) !?
    ( not (T.null (anchorTitle anchor))
    , A.title (textValue (anchorTitle anchor))) <$>
    inlines (anchorDescription anchor)
inline (Note anchor _) = do
  db <- asks metaAnchorFileMap
  let target = internalAnchorTarget db (internalAnchorSwitch anchor)
  H.a ! A.id (textValue (internalAnchorID anchor)) !
    A.class_ "note-ref" !
    A.href (textValue target) <$>
    (H.sup <$> inlines (internalAnchorDescription anchor))

-- Map header level to 'Html' combinator.
heading :: Level -> Html -> Html
heading n
  | n <= 1 = H.h1
  | n == 2 = H.h2
  | n == 3 = H.h3
  | n == 4 = H.h4
  | n == 5 = H.h5
  | otherwise = H.h6

-- Apply a font style.
style :: Style -> Html -> Html
style Normal = H.span ! A.style "font-style: normal;"
style Emph = H.em
style Sub = H.sub
style Sup = H.sup


---------- Adapt for configured HTML version

-- Document type declaration for the configured HTML version.
--
-- Insert doctype declaration and the @\<html\>@ element.
docTypeHtml :: Reader Meta (Html -> Html)
docTypeHtml =
  elFallback H.docTypeHtml docTypeXhtml11 <!>
  attr "xmlns" "http://www.w3.org/1999/xhtml"

-- Insert doctype declaration for XHTML 1.1
-- and the @\<html\>@ element.
docTypeXhtml11 :: Html -> Html
docTypeXhtml11 =
  withXmlDeclaration . withXhtml11Doctype . H1.html

-- Insert @<meta>@ element for @charset@ property.
elMetaCharset :: Reader Meta Html
elMetaCharset =
  (H.meta ! A.charset "utf-8") `elFallback`
  (H1.meta ! A1.httpEquiv "Content-Type" !
    A1.content "text/html; charset=utf-8")

-- Combinator for a @<section>@-like element.
elSection :: Reader Meta (Html -> Html)
elSection = H.section `orDivClass` "section"

-- Combinator for a @<figcaption>@-like element.
elFigcaption :: Reader Meta (Html -> Html)
elFigcaption = H.figcaption `elFallback` (H1.p ! A1.class_ "caption")

-- Combinator for an XHTML5 element with a @<div>@ fallback.
-- The second argument is a CSS class name for the @<div>@ element.
--
-- If not rendering XHTML5, a @<div>@ element with the provided
-- fallback class name is inserted instead of the XHTML5 element.
orDivClass :: (Html -> Html) -> H1.AttributeValue -> Reader Meta (Html -> Html)
orDivClass xhtml5 fallbackClass =
  xhtml5 `elFallback` (H1.div ! A1.class_ fallbackClass)

-- Provide XHTML5 and XHTML1 versions of a value (e.g. an element)
-- to generate a conditional renderer for it.
elFallback :: a -> a -> Reader Meta a
elFallback xhtml5 xhtml1 = do
  htmlVersion <- asks metaWriterHtmlVersion
  case htmlVersion of
    XHTML5 -> return xhtml5
    XHTML1 -> return xhtml1
