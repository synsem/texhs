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

import Control.Applicative
import Control.Monad.Trans.Reader (Reader, runReader, asks)
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
  , string, text, preEscapedText)
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
  runReader (header doc <+> content doc) (docMeta doc)

-- | Convert 'Section' elements to an XML fragment.
sections2xml :: Meta -> [Section] -> LT.Text
sections2xml = convert2xml sections

-- | Convert 'Block' elements to an XML fragment.
--
-- Note: This function does not convert blocks to sections
-- and thus will not create section elements for headers.
-- For a proper rendering of headers, use 'blocks2sections'
-- and 'sections2xml' instead.
blocks2xml :: Meta -> [Block] -> LT.Text
blocks2xml = convert2xml blocks

-- | Convert 'Inline' elements to an XML fragment.
inlines2xml :: Meta -> [Inline] -> LT.Text
inlines2xml = convert2xml inlines

-- Apply renderer to document content.
convert2xml :: (a -> Reader Meta Markup) -> Meta -> a -> LT.Text
convert2xml render meta docdata =
  renderMarkup (runReader (render docdata) meta)


---------- meta

----- header

-- Create TEI header.
header :: SectionDoc -> Reader Meta Markup
header doc = el "teiHeader" <$> fileDesc doc

-- Create @<fileDesc>@ element for TEI header.
fileDesc :: SectionDoc -> Reader Meta Markup
fileDesc doc = el "fileDesc" <$>
  ((el "titleStmt" <$>
     (el "title" ! attr "type" "full" <$>
       ((el "title" ! attr "type" "main" <$> inlines (docTitle doc)) <+>
        if null (docSubTitle doc)
        then memptyR
        else el "title" ! attr "type" "sub" <$> inlines (docSubTitle doc))) <+>
     mconcatR (map (fmap (el "author") . inlines) (docAuthors doc))) <>$
   el "publicationStmt" (el "p" (text "Unknown")) <>$
   el "sourceDesc" (el "p" (text "Born digital.")))

----- bibliography

-- Create section for bibliography.
bibliography :: SectionDoc -> Reader Meta Markup
bibliography doc =
  let citeEntries = sort (M.elems (metaCiteDB (docMeta doc)))
  in if null citeEntries
     then memptyR
     else
       el "div" !
       attr "xml:id" "bibliography" !
       attr "type" "bibliography" <$>
         (el "head" (text "Bibliography") $<>
         -- note: the contained @listBibl@ gets no separate @head@ element
         (el "listBibl" <$> mconcatR (map writeBibEntry citeEntries)))

-- Create a single entry in the bibliography.
writeBibEntry :: CiteEntry -> Reader Meta Markup
writeBibEntry (CiteEntry anchor _ _ formatted) =
  el "bibl" ! attr "xml:id" (textValue (internalAnchorID anchor)) <$>
  inlines formatted


---------- content

-- Create main content as TEI @<text>@ element.
content :: SectionDoc -> Reader Meta Markup
content doc = el "text" <$> front doc <+> body doc <+> back doc


---------- front matter

-- Create front matter as TEI @<front>@ element.
front :: SectionDoc -> Reader Meta Markup
front doc = el "front" <$> titlePage doc

-- Create title page for TEI front matter.
titlePage :: SectionDoc -> Reader Meta Markup
titlePage doc = el "titlePage" <$>
  (el "docTitle" <$>
    (el "titlePart" ! attr "type" "main" <$> inlines (docTitle doc)) <+>
    (if null (docSubTitle doc)
     then memptyR
     else el "titlePart" ! attr "type" "sub" <$> inlines (docSubTitle doc))) <+>
  (el "byline" <$> mconcatR (map (fmap (el "docAuthor") . inlines) (docAuthors doc)))


---------- back matter

-- Create back matter as TEI @<back>@ element.
back :: SectionDoc -> Reader Meta Markup
back doc = el "back" <$> bibliography doc


---------- main matter

-- Create main matter as TEI @<body>@ element.
body :: SectionDoc -> Reader Meta Markup
body (SectionDoc _ docbody) = el "body" <$> sections docbody

-- Convert 'Section' elements to XML.
sections :: [Section] -> Reader Meta Markup
sections = mconcatR . map section

-- Convert 'Block' elements to XML.
blocks :: [Block] -> Reader Meta Markup
blocks = mconcatR . map block

-- Convert 'Inline' elements to XML.
inlines :: [Inline] -> Reader Meta Markup
inlines = mconcatR . map inline

-- Convert a single 'Section' element to XML.
section :: Section -> Reader Meta Markup
section (Section hlevel hanchor htitle secbody subsecs) =
  el "div" !
  attr "xml:id" (textValue (internalAnchorID hanchor)) !
  attr "type" (textValue (levelname hlevel)) <$>
    (el "head" <$> inlines htitle) <+>
    blocks secbody <+>
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
block :: Block -> Reader Meta Markup
block (Para xs) = el "p" <$> inlines xs
block (Header _ _ xs) = el "head" <$> inlines xs
block (List ltype xss) =
  el "list" ! attr "type" (textValue (showListType ltype)) <$>
  mconcatR (map (fmap (el "item") . blocks) xss)
block (ListItemBlock xs) =
  el "list" ! attr "type" "numbered-item-list" <$>
  mconcatR (map listitem xs)
block (QuotationBlock xs) =
  el "quote" <$> blocks xs
block (Figure anchor imgloc imgdesc) =
  el "figure" ! attr "xml:id" (textValue (internalAnchorID anchor)) <$>
  leaf "graphic" ! attr "url" (textValue imgloc) $<>
  (el "head" <$> inlines imgdesc)
block (Table anchor tdesc tdata) =
  el "table" ! attr "xml:id" (textValue (internalAnchorID anchor)) <$>
  (el "head" <$> inlines tdesc) <+>
  mconcatR (map (fmap (el "row") . mconcatR . map tableCell) tdata)
block (SimpleTable tdata) =
  el "table" <$>
  mconcatR (map (fmap (el "row") . mconcatR . map tableCell) tdata)

-- Convert a single 'TableCell' element to XML.
tableCell :: TableCell -> Reader Meta Markup
tableCell (SingleCell xs) =
  el "cell" <$> inlines xs
tableCell (MultiCell i xs) =
  el "cell" ! attr "cols" (stringValue (show i)) <$> inlines xs

-- Convert a single 'ListItem' element to XML.
listitem :: ListItem -> Reader Meta Markup
listitem (ListItem anchor xs) =
  el "item"
    ! attr "xml:id" (textValue (internalAnchorID anchor))
    ! attr "type" "numbered-item"
    ! attr "n" (stringValue (show (internalAnchorLocalNum anchor))) <$>
  blocks xs

-- Convert a single 'Inline' element to XML.
inline :: Inline -> Reader Meta Markup
inline (Str xs) = return $ string xs
inline (FontStyle s xs) = style s <$> inlines xs
inline (Math _ xs) = el "formula" <$> inlines xs
inline Space = return $ text " "
inline (Citation cit _) = do
  db <- asks metaCiteDB
  el "seg" ! attr "type" "citation-group" <$>
    inlines (fmtMultiCite db cit)
inline (Pointer label protoAnchor) = do
  db <- asks metaAnchorMap
  let undefinedAnchor = ExternalResource [Str "???"] "" "" ""
      anchor = fromMaybe undefinedAnchor $ protoAnchor <|>
               (InternalResource <$> M.lookup label db)
  el "ref" !
    attr "target" (textValue (anchorTarget anchor)) !?
    ( not (T.null (anchorType anchor))
    , attr "type" (textValue (anchorType anchor))) <$>
    inlines (anchorDescription anchor)
inline (Note anchor notetext) =
  el "note" !
    attr "xml:id" (textValue (internalAnchorID anchor)) !
    attr "type" "footnote" !
    attr "place" "bottom" !
    attr "n" (stringValue (internalAnchorDescriptionAsText anchor)) <$>
  blocks notetext

-- Apply a font style.
style :: Style -> Markup -> Markup
style Normal = el "hi" ! attr "style" "font-style: normal;"
style Emph = el "emph"
style Sub = el "hi" ! attr "style" "vertical-align: sub; font-size: smaller;"
style Sup = el "hi" ! attr "style" "vertical-align: super; font-size: smaller;"


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


---------- Helper functions for Reader Meta Html

infixl 5 <+>
infixr 5 $<>
infixl 5 <>$

-- Lifted mappend.
(<+>) :: (Applicative f, Monoid a) => f a -> f a -> f a
(<+>) = liftA2 mappend

-- Lifted mappend, apply pure to left value.
($<>) :: (Functor f, Monoid a) => a -> f a -> f a
($<>) = fmap . mappend

-- Lifted mappend, apply pure to right value.
(<>$) :: (Functor f, Monoid a) => f a -> a -> f a
(<>$) = flip (fmap . flip mappend)

-- Lifted mempty.
memptyR :: Reader Meta Markup
memptyR = pure mempty

-- Lifted mconcat.
mconcatR :: [Reader Meta Markup] -> Reader Meta Markup
mconcatR = foldr (<+>) memptyR
