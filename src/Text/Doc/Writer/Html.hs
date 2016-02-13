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
import Control.Monad.Trans.Reader (Reader, runReader, asks)
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
import Text.Doc.Writer.Core


---------- main: Doc to HTML conversion

-- | Convert a 'Doc' document to HTML.
doc2html :: Doc -> Text
doc2html = renderHtml . convertDoc

-- Convert 'Doc' to 'Html' type.
convertDoc :: Doc -> Html
convertDoc doc = docTypeHtml $
  runReader (mkHead doc <+> mkBody doc) (docMeta doc)

-- | Convert 'Block' elements to an HTML fragment.
blocks2html :: Meta -> [Block] -> Text
blocks2html = convert2html blocks

-- | Convert 'Inline' elements to an HTML fragment.
inlines2html :: Meta -> [Inline] -> Text
inlines2html = convert2html inlines

-- Apply renderer to document content.
convert2html :: (a -> Reader Meta Html) -> Meta -> a -> Text
convert2html render meta docdata =
  renderHtml (runReader (render docdata) meta)


---------- meta

-- Create @<head>@ element.
mkHead :: Doc -> Reader Meta Html
mkHead doc = H.head <$>
  (((H.meta ! charset "utf-8") $<>
    (H.title <$> inlines (docTitle doc))) <>$
   (H.meta ! name "viewport" ! content "width=device-width, initial-scale=1.0") <>$
   (H.meta ! name "generator" ! content "texhs"))

----- header

-- Create @<header>@ element.
header :: Doc -> Reader Meta Html
header doc = H.header <$>
  ((h1 ! A.class_ "title" <$> inlines (docTitle doc)) <+>
  unlessR (null (docSubTitle doc))
    (h1 ! A.class_ "subtitle" <$> inlines (docSubTitle doc)) <+>
  (h2 ! A.class_ "author" <$> foldMapR inlines (docAuthors doc)))

----- toc

-- Create a table of contents.
toc :: Doc -> Reader Meta Html
toc doc =
  let (SectionDoc meta secs) = doc2secdoc doc
      noteEntry = li $ a ! href "#footnotes" $ "Footnotes"
      biblEntry = li $ a ! href "#bibliography" $ "Bibliography"
  in unlessR (null secs)
     (H.nav ! A.id "toc" <$> (ul <$>
       (foldMapR tocEntry secs <>$
        unlessR (M.null (metaNoteMap meta)) noteEntry <>
        unlessR (M.null (metaCiteDB meta)) biblEntry)))

-- Create a toc entry for a single section (and its subsections).
tocEntry :: Section -> Reader Meta Html
tocEntry (Section _ anchor title _ subsecs) = do
  db <- asks metaAnchorFileMap
  li <$>
    ((a ! href (textValue (internalAnchorTarget db anchor)) <$>
      (sectionNumberPrefix anchor <+> inlines title)) <+>
    unlessR (null subsecs)
      (ul <$> foldMapR tocEntry subsecs))

-- Create a section number.
--
-- Includes a trailing space, ready to be prefixed to a section title.
-- For phantom (unnumbered) sections, the generated prefix is empty.
sectionNumberPrefix :: InternalAnchor -> Reader Meta Html
sectionNumberPrefix anchor@(SectionAnchor _) =
  unlessR (isPhantomSection anchor)
    (inlines (internalAnchorDescription anchor) <>$ toHtml ' ')
sectionNumberPrefix _ = memptyR

----- footnotes

-- Create section for footnotes.
footnotes :: Doc -> Reader Meta Html
footnotes (Doc meta _) =
  let notes = metaNoteMap meta
      keys = M.keys notes             -- 'M.keys' returns sorted list
      chapters = (nub . map fst) keys -- only chapters with notes
  in unlessR (null keys)
       ((h1 ! A.id "footnotes" $ "Footnotes") $<>
        foldMapR (footnotesForChapter notes) chapters)

-- Create footnotes for a given chapter number.
footnotesForChapter :: Map (Int, Int) [Block] -> Int -> Reader Meta Html
footnotesForChapter notes chapnum =
  let chap = T.pack (show chapnum)
      headerID = textValue (T.append "footnotes-chap-" chap)
      headerTitle = toHtml (T.append "Chapter " chap)
      fndata = filter ((chapnum==) . fst . fst) (M.assocs notes)
  in (h2 ! A.id headerID $ headerTitle) $<>
     (ol <$> foldMapR (footnote . first NoteAnchor) fndata)

-- Create a single footnote.
footnote :: (InternalAnchor, [Block]) -> Reader Meta Html
footnote (anchor, fntext) =
  li ! A.id (textValue (internalAnchorID anchor)) <$>
    blocks fntext <+>
    backreference anchor

-- Create a backreference to an anchor.
--
-- For example, insert a backreference into a footnote text
-- in order to refer back to the corresponding footnote mark.
backreference :: InternalAnchor -> Reader Meta Html
backreference anchor = do
  let backrefText = "^"
  db <- asks metaAnchorFileMap
  return $ p $ a ! A.class_ "note-backref" !
     href (textValue (internalAnchorTargetRef db anchor)) $
     backrefText

----- bibliography

-- Create section for bibliography.
bibliography :: Doc -> Reader Meta Html
bibliography (Doc meta _) =
  let citeEntries = sort (M.elems (metaCiteDB meta))
  in unlessR (null citeEntries)
       ((h1 ! A.id "bibliography" $ "Bibliography") $<>
        (ol ! A.id "biblist" <$> foldMapR writeBibEntry citeEntries))

-- Create a single entry in the bibliography.
writeBibEntry :: CiteEntry -> Reader Meta Html
writeBibEntry (CiteEntry anchor _ _ formatted) =
  li ! A.id (textValue (internalAnchorID anchor)) <$>
  inlines formatted


---------- content

-- Create @<body>@ element.
mkBody :: Doc -> Reader Meta Html
mkBody doc@(Doc _ docbody) =
  H.body <$>
    (header doc <+>
     toc doc <+>
     (H.main <$>
       (blocks docbody <+>
        footnotes doc <+>
        bibliography doc)))

-- Convert 'Block' elements to HTML.
blocks :: [Block] -> Reader Meta Html
blocks = foldMapR block

-- Convert 'Inline' elements to HTML.
inlines :: [Inline] -> Reader Meta Html
inlines = foldMapR inline

-- Convert a single 'Block' element to HTML.
block :: Block -> Reader Meta Html
block (Para xs) = p <$> inlines xs
block (Header level anchor xs) =
  heading level ! A.id (textValue (internalAnchorID anchor)) <$>
  (sectionNumberPrefix anchor <+> inlines xs)
block (List UnorderedList xss) =
  ul <$> foldMapR (fmap li . blocks) xss
block (List OrderedList xss) =
  ol <$> foldMapR (fmap li . blocks) xss
block (ListItemBlock xs) =
  ol ! A.class_ "numbered-item-list" <$>
  foldMapR listitem xs
block (QuotationBlock xs) = H.blockquote <$> blocks xs
block (Figure anchor imgloc imgdesc) =
  H.figure ! A.id (textValue (internalAnchorID anchor)) <$>
    (H.img ! A.src (textValue imgloc) $<>
     H.figcaption <$>
       ((text "Figure " $<>
         inlines (internalAnchorDescription anchor)) <+>
        (text ": " $<>
         inlines imgdesc)))
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
  H.td ! A.colspan (H.stringValue (show i)) <$> inlines xs

-- Convert a single 'ListItem' element to HTML.
listitem :: ListItem -> Reader Meta Html
listitem (ListItem anchor xs) =
  li ! A.id (textValue (internalAnchorID anchor))
     ! A.class_ "numbered-item"
     ! A.value (H.stringValue (show (internalAnchorLocalNum anchor))) <$>
    blocks xs

-- Convert a single 'Inline' element to HTML.
inline :: Inline -> Reader Meta Html
inline (Str xs) = return $ toHtml xs
inline (FontStyle s xs) = style s <$> inlines xs
inline (Math _ xs) =
  H.span ! A.class_ "math" <$>
  inlines xs
inline Space = return $ toHtml ' '
inline (Citation cit) = do
  db <- asks metaCiteDB
  H.span ! A.class_ "citation-group" <$>
    inlines (fmtMultiCite db cit)
inline (Pointer label protoAnchor) = do
  anchorDB <- asks metaAnchorMap
  fileDB <- asks metaAnchorFileMap
  let anchor = extractAnchor anchorDB label protoAnchor
  a ! href (textValue (anchorTarget fileDB anchor))
    !? ( not (T.null (anchorType anchor))
       , A.class_ (textValue (anchorType anchor)))
    !? ( not (T.null (anchorTitle anchor))
       , A.title (textValue (anchorTitle anchor))) <$>
    inlines (anchorDescription anchor)
inline (Note anchor _) = do
  db <- asks metaAnchorFileMap
  a ! A.id (textValue (internalAnchorIDRef anchor))
    ! A.class_ "note-ref"
    ! href (textValue (internalAnchorTarget db anchor)) <$>
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
