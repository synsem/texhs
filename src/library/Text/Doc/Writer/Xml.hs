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
-- Portability :  portable
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


---------- main: Doc to XML conversion

-- | Convert a 'Doc' document to a TEI-based XML format.
doc2xml :: Doc -> LT.Text
doc2xml = renderMarkup . convertDoc

-- Convert 'Doc' to XML 'Markup'.
convertDoc :: Doc -> Markup
convertDoc doc =
  el "TEI" ! (attr "xmlns" "http://www.tei-c.org/ns/1.0") $
  header doc <> content doc


---------- meta

-- Create TEI header.
header :: Doc -> Markup
header doc = el "teiHeader" $ fileDesc doc

-- Create @<fileDesc>@ element for TEI header.
fileDesc :: Doc -> Markup
fileDesc doc = el "fileDesc" $ do
  el "titleStmt" $ do
    el "title" (textP (docTitle doc))
    el "author" (textP (docAuthor doc))
  el "publicationStmt" $ p (text "Unknown")
  el "sourceDesc" $ p (text "Born digital.")


---------- content

-- Create main content as TEI @<text>@ element.
content :: Doc -> Markup
content doc = el "text" $ front doc <> body doc <> back doc


---------- front matter

-- Create front matter as TEI @<front>@ element.
front :: Doc -> Markup
front doc = el "front" (titlePage doc)

-- Create title page for TEI front matter.
titlePage :: Doc -> Markup
titlePage doc = el "titlePage" $ do
  el "docTitle" $
    el "titlePart" ! attr "type" "main" $ (textP (docTitle doc))
  el "byline" $ el "docAuthor" (textP (docAuthor doc))


---------- back matter

-- Create back matter as TEI @<back>@ element.
back :: Doc -> Markup
back _ = leaf "back"


---------- main matter

-- Create main matter as TEI @<body>@ element.
body :: Doc -> Markup
body (Doc _ docbody) = el "body" $ blocks docbody

-- Convert 'Block' elements to XML.
blocks :: [Block] -> Markup
blocks = mapM_ block

-- Convert 'Inline' elements to XML.
inlines :: [Inline] -> Markup
inlines = mapM_ inline

-- Convert a single 'Block' element to XML.
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
