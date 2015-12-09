{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.Doc.Writer.Xml
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
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
 ) where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Text.Blaze.Internal
import Text.Blaze.Renderer.Text (renderMarkup)

import Text.Doc.Types
import Text.Doc.Section


---------- main: Doc to XML conversion

-- | Convert a 'Doc' document to a TEI-based XML format.
doc2xml :: Doc -> LT.Text
doc2xml = renderMarkup . convertDoc . doc2secdoc

-- Convert 'SectionDoc' to XML 'Markup'.
convertDoc :: SectionDoc -> Markup
convertDoc doc =
  el "TEI" ! (attr "xmlns" "http://www.tei-c.org/ns/1.0") $
  header doc <> content doc


---------- meta

-- Create TEI header.
header :: SectionDoc -> Markup
header doc = el "teiHeader" $ fileDesc doc

-- Create @<fileDesc>@ element for TEI header.
fileDesc :: SectionDoc -> Markup
fileDesc doc = el "fileDesc" $ do
  el "titleStmt" $ do
    el "title" (textP (sdocTitle doc))
    el "author" (textP (sdocAuthor doc))
  el "publicationStmt" $ p (text "Unknown")
  el "sourceDesc" $ p (text "Born digital.")


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
    el "titlePart" ! attr "type" "main" $ (textP (sdocTitle doc))
  el "byline" $ el "docAuthor" (textP (sdocAuthor doc))


---------- back matter

-- Create back matter as TEI @<back>@ element.
back :: SectionDoc -> Markup
back _ = leaf "back"


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
section (Section hlevel htitle secbody subsecs) =
  el "div" ! attr "type" (textValue (levelname hlevel)) $ do
    el "head" $ inlines htitle
    blocks secbody
    sections subsecs

-- Convert a numeric header level to a textual description.
levelname :: Level -> Text
levelname i = maybe "unknown" id (lookup i (zip [1..] levels))
  where
    levels = [ "part", "chapter", "section"
             , "subsection", "subsubsection" ]

-- Convert a single 'Block' element to XML.
--
-- Note: SectionDoc documents should not contain Header elements,
-- all header information is collected in Section elements.
block :: Block -> Markup
block (Para xs) = p $ inlines xs
block (Header _ xs) = el "head" $ inlines xs
block (List xss) = el "list" $ mapM_ (el "item" . blocks) xss

-- Convert a single 'Inline' element to XML.
inline :: Inline -> Markup
inline (Str xs) = textP xs
inline (Normal xs) =
  el "hi" ! attr "style" "font-style: normal;" $
  inlines xs
inline (Emph xs) = el "emph" $ inlines xs
inline Space = textP " "


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
