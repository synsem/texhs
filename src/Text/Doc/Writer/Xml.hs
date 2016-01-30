{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.Doc.Writer.Xml
-- Copyright   :  2015-2016 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  GHC
--
-- XML writer: Convert Doc to a TEI-based XML format.
----------------------------------------------------------------------

module Text.Doc.Writer.Xml
 ( -- * Doc to XML Conversion
   doc2xml
 , sections2xml
 , blocks2xml
 , inlines2xml
 ) where

import Control.Monad (unless)
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Text.Blaze.Internal
  ( Markup, customLeaf, customParent, textTag
  , Attribute, AttributeValue, (!), (!?), attribute, textValue, stringValue
  , text, preEscapedText)
import Text.Blaze.Renderer.Text (renderMarkup)

import Text.Bib.Writer
import Text.Doc.Types
import Text.Doc.Section


---------- main: Doc to XML conversion

-- | Convert a 'Doc' document to a TEI-based XML format.
doc2xml :: Doc -> LT.Text
doc2xml = renderMarkup . convertDoc . doc2secdoc

-- Convert 'SectionDoc' to XML 'Markup'.
convertDoc :: SectionDoc -> Markup
convertDoc doc =
  (preEscapedText "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" <>) $
  el "TEI" ! attr "xmlns" "http://www.tei-c.org/ns/1.0" $
  header doc <> content doc

-- | Convert 'Section' elements to an XML fragment.
sections2xml :: [Section] -> LT.Text
sections2xml = renderMarkup . sections

-- | Convert 'Block' elements to an XML fragment.
--
-- Note: This function does not convert blocks to sections
-- and thus will not create section elements for headers.
-- For a proper rendering of headers, use 'blocks2sections'
-- and 'sections2xml' instead.
blocks2xml :: [Block] -> LT.Text
blocks2xml = renderMarkup . blocks

-- | Convert 'Inline' elements to an XML fragment.
inlines2xml :: [Inline] -> LT.Text
inlines2xml = renderMarkup . inlines


---------- meta

----- header

-- Create TEI header.
header :: SectionDoc -> Markup
header doc = el "teiHeader" $ fileDesc doc

-- Create @<fileDesc>@ element for TEI header.
fileDesc :: SectionDoc -> Markup
fileDesc doc = el "fileDesc" $ do
  el "titleStmt" $ do
    el "title" (inlines (docTitle doc))
    mapM_ (el "author" . inlines) (docAuthors doc)
  el "publicationStmt" $ p (text "Unknown")
  el "sourceDesc" $ p (text "Born digital.")

----- bibliography

-- Create section for bibliography.
bibliography :: SectionDoc -> Markup
bibliography doc =
  let citeEntries = sort (M.elems (metaCiteDB (docMeta doc)))
  in unless (null citeEntries) $
     el "div" !
     attr "xml:id" "bibliography" !
     attr "type" "bibliography" $
       el "head" (text "Bibliography") <>
       -- note: the contained @listBibl@ gets no separate @head@ element
       el "listBibl" (mapM_ writeBibEntry citeEntries)

-- Create a single entry in the bibliography.
writeBibEntry :: CiteEntry -> Markup
writeBibEntry (CiteEntry anchor _ _ formatted) =
  el "bibl" ! attr "xml:id" (textValue (internalAnchorID anchor)) $
  inlines formatted


---------- content

-- Create main content as TEI @<text>@ element.
content :: SectionDoc -> Markup
content doc = el "text" $ front doc <> body doc <> back doc


---------- front matter

-- Create front matter as TEI @<front>@ element.
front :: SectionDoc -> Markup
front doc = el "front" (titlePage doc)

-- Create title page for TEI front matter.
titlePage :: SectionDoc -> Markup
titlePage doc = el "titlePage" $ do
  el "docTitle" $
    el "titlePart" ! attr "type" "main" $ inlines (docTitle doc)
  el "byline" $ mapM_ (el "docAuthor" . inlines) (docAuthors doc)


---------- back matter

-- Create back matter as TEI @<back>@ element.
back :: SectionDoc -> Markup
back doc = el "back" (bibliography doc)


---------- main matter

-- Create main matter as TEI @<body>@ element.
body :: SectionDoc -> Markup
body (SectionDoc _ docbody) = el "body" $ sections docbody

-- Convert 'Section' elements to XML.
sections :: [Section] -> Markup
sections = mapM_ section

-- Convert 'Block' elements to XML.
blocks :: [Block] -> Markup
blocks = mapM_ block

-- Convert 'Inline' elements to XML.
inlines :: [Inline] -> Markup
inlines = mapM_ inline

-- Convert a single 'Section' element to XML.
section :: Section -> Markup
section (Section hlevel hanchor htitle secbody subsecs) =
  el "div" !
  attr "xml:id" (textValue (internalAnchorID hanchor)) !
  attr "type" (textValue (levelname hlevel)) $ do
    el "head" $ inlines htitle
    blocks secbody
    sections subsecs

-- Convert a numeric header level to a textual description.
levelname :: Level -> Text
levelname i = fromMaybe "unknown" (lookup i (zip [1..] levels))
  where
    levels = [ "part", "chapter", "section"
             , "subsection", "subsubsection" ]

-- Convert a list type to a textual description.
showListType :: ListType -> Text
showListType UnorderedList = "unordered"
showListType OrderedList = "ordered"

-- Convert a single 'Block' element to XML.
--
-- Note: SectionDoc documents should not contain Header elements,
-- all header information is collected in Section elements.
block :: Block -> Markup
block (Para xs) = p $ inlines xs
block (Header _ _ xs) = el "head" $ inlines xs
block (List ltype xss) =
  el "list" ! attr "type" (textValue (showListType ltype)) $
  mapM_ (el "item" . blocks) xss
block (ListItemBlock xs) =
  el "list" ! attr "type" "numbered-item-list" $
  mapM_ listitem xs
block (QuotationBlock xs) =
  el "quote" $ blocks xs
block (Figure anchor imgloc imgdesc) =
  el "figure" ! attr "xml:id" (textValue (internalAnchorID anchor)) $
  leaf "graphic" ! attr "url" (textValue imgloc) <>
  el "head" (inlines imgdesc)
block (Table anchor tdesc tdata) =
  el "table" ! attr "xml:id" (textValue (internalAnchorID anchor)) $
  el "head" (inlines tdesc) <>
  mapM_ (el "row" . mapM_ tableCell) tdata
block (SimpleTable tdata) =
  el "table" $
  mapM_ (el "row" . mapM_ tableCell) tdata

-- Convert a single 'TableCell' element to XML.
tableCell :: TableCell -> Markup
tableCell (SingleCell xs) =
  el "cell" $ inlines xs
tableCell (MultiCell i xs) =
  el "cell" ! attr "cols" (stringValue (show i)) $ inlines xs

-- Convert a single 'ListItem' element to XML.
listitem :: ListItem -> Markup
listitem (ListItem anchor xs) =
  el "item"
    ! attr "xml:id" (textValue (internalAnchorID anchor))
    ! attr "type" "numbered-item"
    ! attr "n" (stringValue (show (internalAnchorLocalNum anchor))) $
  blocks xs

-- Convert a single 'Inline' element to XML.
inline :: Inline -> Markup
inline (Str xs) = textP xs
inline (FontStyle s xs) = style s $ inlines xs
inline (Math _ xs) =
  el "formula" $ inlines xs
inline Space = text " "
inline (Citation _ Nothing) =
  error "XML Writer does not support unprocessed citations."
inline (Citation cit (Just db)) =
  el "seg" ! attr "type" "citation-group" $
  inlines (fmtMultiCite db cit)
inline (Pointer _ Nothing) =
  error "XML Writer does not support unprocessed or undefined pointers."
inline (Pointer _ (Just anchor)) =
  el "ref"
     ! attr "target" (textValue (anchorTarget anchor))
     !? ( not (T.null (anchorType anchor))
        , attr "type" (textValue (anchorType anchor))) $
  inlines (anchorDescription anchor)
inline (Note anchor notetext) =
  el "note" !
    attr "xml:id" (textValue (internalAnchorID anchor)) !
    attr "type" "footnote" !
    attr "place" "bottom" !
    attr "n" (stringValue (internalAnchorDescriptionAsText anchor)) $
  blocks notetext

-- Apply a font style.
style :: Style -> Markup -> Markup
style Normal = el "hi" ! attr "style" "font-style: normal;"
style Emph = el "emph"
style Sub = el "hi" ! attr "style" "vertical-align: sub; font-size: smaller;"
style Sup = el "hi" ! attr "style" "vertical-align: super; font-size: smaller;"


---------- String to text helper functions

-- Create Markup via Text from String.
textP :: String -> Markup
textP = text . T.pack


---------- XML markup shortcuts

-- Create @<p>@ element.
p :: Markup -> Markup
p = el "p"


---------- Helper functions for creating XML Markup

-- Create element with content from tag name.
el :: Text -> Markup -> Markup
el = customParent . textTag

-- Create self-closing empty element from tag name.
leaf :: Text -> Markup
leaf = flip customLeaf True . textTag

-- Create attribute name, expecting an attribute value.
attr :: Text -> AttributeValue -> Attribute
attr n = attribute (textTag n) (textTag (" " <> n <> "=\""))
