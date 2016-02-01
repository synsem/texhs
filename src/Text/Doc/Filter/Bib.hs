{-# LANGUAGE CPP #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.Doc.Filter.Bib
-- Copyright   :  2015-2016 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  GHC
--
-- Document filter: Resolve bibliographic references in a document
-- relative to a bibliographic database.
----------------------------------------------------------------------

module Text.Doc.Filter.Bib
 ( -- * Bib Filter
   -- ** Meta
   docBibFilter
   -- ** Content
 , distributeMeta
 ) where

#if MIN_VERSION_base(4,8,0)
-- Prelude exports all required operators from Control.Applicative
#else
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Monad.Trans.Reader
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)

import Text.Bib (BibDB, resolveCitations)
import Text.Doc.Types


-------------------- Meta

-- | Resolve bibliographic references in a document ('Doc')
-- relative to a bibliographic database ('BibDB').
docBibFilter :: BibDB -> Doc -> Doc
docBibFilter db (Doc meta content) =
  Doc (populateCiteDB db meta) content

-- | Resolve citekeys in a Meta record.
--
-- This function stitches together
-- citekeys (typically extracted from a texfile) and
-- bibliographic entries (typically extracted from a bibfile).
populateCiteDB :: BibDB -> Meta -> Meta
populateCiteDB db meta =
  let citeDB = resolveCitations db (metaCiteMap meta)
  in meta { metaCiteDB = citeDB }


-------------------- Content

-- NOTE: This processing step is intended to help writers
-- that cannot easily run on top of a Reader monad themselves
-- and thus have no online access to document meta information
-- (e.g. writers based on blaze-markup or blaze-html).

-- | Distribute global document information from Meta
-- to content elements (blocks and inlines).
--
-- In particular, inject data into the second arguments of
-- 'Citation' and 'Pointer' inline elements.
distributeMeta :: Doc -> Doc
distributeMeta (Doc meta content) =
  let distributeTo = flip runReader meta . distributeMetaToContent
      -- Content elements within 'Meta' must be processed as well.
      meta' = meta { metaNoteMap = M.map distributeTo (metaNoteMap meta) }
  in Doc meta' (distributeTo content)

distributeMetaToContent :: Content -> Reader Meta Content
distributeMetaToContent = mapM distributeMetaToBlock

distributeMetaToBlock :: Block -> Reader Meta Block
distributeMetaToBlock (Para xs) =
  Para <$> mapM distributeMetaToInline xs
distributeMetaToBlock (Header l a xs) =
  Header l a <$> mapM distributeMetaToInline xs
distributeMetaToBlock (List ltype xss) =
  List ltype <$> mapM (mapM distributeMetaToBlock) xss
distributeMetaToBlock (ListItemBlock xs) =
  ListItemBlock <$> mapM distributeMetaToListItem xs
distributeMetaToBlock (QuotationBlock xs) =
  QuotationBlock <$> mapM distributeMetaToBlock xs
distributeMetaToBlock (Figure a l xs) =
  Figure a l <$> mapM distributeMetaToInline xs
distributeMetaToBlock (Table a xs rows) =
  Table a <$>
    mapM distributeMetaToInline xs <*>
    mapM (mapM distributeMetaToTableCell) rows
distributeMetaToBlock (SimpleTable rows) =
  SimpleTable <$> mapM (mapM distributeMetaToTableCell) rows

distributeMetaToListItem :: ListItem -> Reader Meta ListItem
distributeMetaToListItem (ListItem a bs) =
  ListItem a <$> mapM distributeMetaToBlock bs

distributeMetaToTableCell :: TableCell -> Reader Meta TableCell
distributeMetaToTableCell (SingleCell xs) =
  SingleCell <$> mapM distributeMetaToInline xs
distributeMetaToTableCell (MultiCell i xs) =
  MultiCell i <$> mapM distributeMetaToInline xs

distributeMetaToInline :: Inline -> Reader Meta Inline
distributeMetaToInline (Str xs) =
  return $ Str xs
distributeMetaToInline (FontStyle s xs) =
  FontStyle s <$> mapM distributeMetaToInline xs
distributeMetaToInline (Math t xs) =
  Math t <$> mapM distributeMetaToInline xs
distributeMetaToInline Space =
  return Space
distributeMetaToInline (Citation cit _) =
  asks metaCiteDB >>= \ db ->
  return $ Citation cit (Just (M.fromList (mapMaybe (\ key ->
    (,) key <$> M.lookup key db) (extractCiteKeys cit))))
distributeMetaToInline (Pointer label Nothing) =
  asks metaAnchorMap >>= \ db ->
  return $ Pointer label (InternalResource <$> M.lookup label db)
distributeMetaToInline p@(Pointer _ (Just _)) =
  return p
distributeMetaToInline (Note a bs) =
  Note a <$> mapM distributeMetaToBlock bs
