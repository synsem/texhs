{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.Doc.Writer.Html
-- Copyright   :  2015-2016 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  GHC
--
-- HTML writer: Convert Doc to HTML.
----------------------------------------------------------------------

module Text.Doc.Writer.Html
 ( -- * Doc to HTML Conversion
   doc2html
 , blocks2html
 , inlines2html
 ) where

import Control.Arrow (first)
import Control.Monad (unless)
import Data.List (nub, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Text.Lazy (Text)
import qualified Data.Text as T
import Text.Blaze.Html5
  ( (!), (!?), Html, toHtml, text, textValue, docTypeHtml
  , h1, h2, ul, ol, li, p, a)
import Text.Blaze.Html5.Attributes
  (name, charset, content, href)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Bib.Writer
import Text.Doc.Types
import Text.Doc.Section


---------- main: Doc to HTML conversion

-- | Convert a 'Doc' document to HTML.
doc2html :: Doc -> Text
doc2html = renderHtml . convertDoc

-- Convert 'Doc' to 'Html' type.
convertDoc :: Doc -> Html
convertDoc doc = docTypeHtml $ mkHead doc <> mkBody doc

-- | Convert 'Block' elements to an HTML fragment.
blocks2html :: [Block] -> Text
blocks2html = renderHtml . blocks

-- | Convert 'Inline' elements to an HTML fragment.
inlines2html :: [Inline] -> Text
inlines2html = renderHtml . inlines


---------- meta

-- Create @<head>@ element.
mkHead :: Doc -> Html
mkHead doc = H.head $ do
  H.meta ! charset "utf-8"
  H.title $ inlines (docTitle doc)
  H.meta ! name "viewport" ! content "width=device-width, initial-scale=1.0"
  H.meta ! name "generator" ! content "texhs"

----- header

-- Create @<header>@ element.
header :: Doc -> Html
header doc = H.header $ do
  h1 ! A.class_ "title" $ inlines (docTitle doc)
  h2 ! A.class_ "author" $ mapM_ inlines (docAuthors doc)

----- toc

-- Create a table of contents.
toc :: Doc -> Html
toc doc =
  let (SectionDoc meta secs) = doc2secdoc doc
  in unless (null secs) $
       H.nav ! A.id "toc" $
       ul $ do
         mapM_ tocEntry secs
         unless (M.null (metaNoteMap meta))
           (li $ a ! href "#footnotes" $ "Footnotes")
         unless (M.null (metaCiteDB meta))
           (li $ a ! href "#bibliography" $ "Bibliography")

-- Create a toc entry for a single section (and its subsections).
tocEntry :: Section -> Html
tocEntry (Section _ anchor title _ subsecs) = do
  li $ a ! href (textValue (internalAnchorTarget anchor)) $
    sectionNumberPrefix anchor <> inlines title
  unless (null subsecs) $
    ul $ mapM_ tocEntry subsecs

-- Create a section number.
--
-- Includes a trailing space, ready to be prefixed to a section title.
-- For phantom (unnumbered) sections, the generated prefix is empty.
sectionNumberPrefix :: InternalAnchor -> Html
sectionNumberPrefix anchor@(SectionAnchor _) =
  unless (isPhantomSection anchor)
    (inlines (internalAnchorDescription anchor) <> toHtml ' ')
sectionNumberPrefix _ = mempty

----- footnotes

-- Create section for footnotes.
footnotes :: Doc -> Html
footnotes (Doc meta _) =
  let notes = metaNoteMap meta
      keys = M.keys notes             -- 'M.keys' returns sorted list
      chapters = (nub . map fst) keys -- only chapters with notes
  in unless (null keys) $
    (h1 ! A.id "footnotes" $ "Footnotes") <>
    mapM_ (footnotesForChapter notes) chapters

-- Create footnotes for a given chapter number.
footnotesForChapter :: Map (Int, Int) [Block] -> Int -> Html
footnotesForChapter notes chapnum =
  let chap = T.pack (show chapnum)
      headerID = textValue (T.append "footnotes-chap-" chap)
      headerTitle = toHtml (T.append "Chapter " chap)
      fndata = filter ((chapnum==) . fst . fst) (M.assocs notes)
  in (h2 ! A.id headerID $ headerTitle) <>
     ol (mapM_ (footnote . first NoteAnchor) fndata)

-- Create a single footnote.
footnote :: (InternalAnchor, [Block]) -> Html
footnote (anchor, fntext) =
  li ! A.id (textValue (internalAnchorID anchor)) $
  blocks fntext <>
  backreference anchor

-- Create a backreference to an anchor.
--
-- For example, insert a backreference into a footnote text
-- in order to refer back to the corresponding footnote mark.
backreference :: InternalAnchor -> Html
backreference anchor =
  let backrefText = "^"
  in p $ a ! A.class_ "note-backref"
           ! href (textValue (internalAnchorTargetRef anchor)) $
     backrefText

----- bibliography

-- Create section for bibliography.
bibliography :: Doc -> Html
bibliography (Doc meta _) =
  let citeEntries = sort (M.elems (metaCiteDB meta))
  in unless (null citeEntries) $
     (h1 ! A.id "bibliography" $ "Bibliography") <>
     (ol ! A.id "biblist" $ mapM_ writeBibEntry citeEntries)

-- Create a single entry in the bibliography.
writeBibEntry :: CiteEntry -> Html
writeBibEntry (CiteEntry anchor _ _ formatted) =
  li ! A.id (textValue (internalAnchorID anchor)) $
  inlines formatted


---------- content

-- Create @<body>@ element.
mkBody :: Doc -> Html
mkBody doc@(Doc _ docbody) = H.body $ do
  header doc
  toc doc
  H.main $ do
    blocks docbody
    footnotes doc
    bibliography doc

-- Convert 'Block' elements to HTML.
blocks :: [Block] -> Html
blocks = mapM_ block

-- Convert 'Inline' elements to HTML.
inlines :: [Inline] -> Html
inlines = mapM_ inline

-- Convert a single 'Block' element to HTML.
block :: Block -> Html
block (Para xs) = p $ inlines xs
block (Header level anchor xs) =
  heading level ! A.id (textValue (internalAnchorID anchor)) $
  sectionNumberPrefix anchor <> inlines xs
block (List UnorderedList xss) = ul $ mapM_ (li . blocks) xss
block (List OrderedList xss) = ol $ mapM_ (li . blocks) xss
block (ListItemBlock xs) =
  ol ! A.class_ "numbered-item-list" $
  mapM_ listitem xs
block (QuotationBlock xs) = H.blockquote $ blocks xs
block (Figure anchor imgloc imgdesc) =
  H.figure ! A.id (textValue (internalAnchorID anchor)) $
  H.img ! A.src (textValue imgloc) <>
  H.figcaption (mconcat
    [ text "Figure "
    , inlines (internalAnchorDescription anchor)
    , text ": "
    , inlines imgdesc ])
block (Table anchor tdesc tdata) =
  H.table ! A.id (textValue (internalAnchorID anchor)) $
  H.caption (mconcat
    [ text "Table "
    , inlines (internalAnchorDescription anchor)
    , text ": "
    , inlines tdesc ]) <>
  H.tbody (mapM_ (H.tr . mapM_ tableCell) tdata)
block (SimpleTable tdata) =
  H.table $
  H.tbody (mapM_ (H.tr . mapM_ tableCell) tdata)

-- Convert a single 'TableCell' element to HTML.
tableCell :: TableCell -> Html
tableCell (SingleCell xs) =
  H.td $ inlines xs
tableCell (MultiCell i xs) =
  H.td ! A.colspan (H.stringValue (show i)) $ inlines xs

-- Convert a single 'ListItem' element to HTML.
listitem :: ListItem -> Html
listitem (ListItem anchor xs) =
  li ! A.id (textValue (internalAnchorID anchor))
     ! A.class_ "numbered-item"
     ! A.value (H.stringValue (show (internalAnchorLocalNum anchor))) $
  blocks xs

-- Convert a single 'Inline' element to HTML.
inline :: Inline -> Html
inline (Str xs) = toHtml xs
inline (FontStyle s xs) = style s $ inlines xs
inline (Math _ xs) =
  H.span ! A.class_ "math" $
  inlines xs
inline Space = toHtml ' '
inline (Citation _ Nothing) =
  error "HTML Writer does not support unprocessed citations."
inline (Citation cit (Just db)) =
  H.span ! A.class_ "citation-group" $
  inlines (fmtMultiCite db cit)
inline (Pointer _ Nothing) =
  error "HTML Writer does not support unprocessed or undefined pointers."
inline (Pointer _ (Just anchor)) =
  a ! href (textValue (anchorTarget anchor))
    !? ( not (T.null (anchorType anchor))
       , A.class_ (textValue (anchorType anchor)))
    !? ( not (T.null (anchorTitle anchor))
       , A.title (textValue (anchorTitle anchor))) $
  inlines (anchorDescription anchor)
inline (Note anchor _) =
  a ! A.id (textValue (internalAnchorIDRef anchor))
    ! A.class_ "note-ref"
    ! href (textValue (internalAnchorTarget anchor)) $
  H.sup $ inlines (internalAnchorDescription anchor)

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
