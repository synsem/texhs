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

import Control.Monad.Trans.Reader
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)

import Text.Bib.Types (BibDB)
import Text.Bib.Writer (resolveCitations, fmtCiteEntries)
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
  let keys = M.keys (metaCiteMap meta)
      citeDB = resolveCitations db keys
  in meta { metaCiteDB = citeDB }


-------------------- Content

-- NOTE: This processing step is intended to help writers
-- that cannot easily run on top of a Reader monad themselves
-- and thus have no online access to document meta information
-- (e.g. writers based on blaze-markup or blaze-html).

-- | Distribute document information from Meta to content elements
-- (blocks and inlines).
--
-- In particular, inject formatted versions of citations into the
-- second argument of 'Citation' inlines.
distributeMeta :: Doc -> Doc
distributeMeta (Doc meta content) =
  Doc meta (runReader (distributeMetaToContent content) meta)

distributeMetaToContent :: Content -> Reader Meta Content
distributeMetaToContent content =
  mapM distributeMetaToBlock content

distributeMetaToBlock :: Block -> Reader Meta Block
distributeMetaToBlock (Para xs) =
  Para <$> mapM distributeMetaToInline xs
distributeMetaToBlock (Header l xs) =
  Header l <$> mapM distributeMetaToInline xs
distributeMetaToBlock (List xss) =
  List <$> mapM (mapM distributeMetaToBlock) xss

distributeMetaToInline :: Inline -> Reader Meta Inline
distributeMetaToInline (Str xs) =
  return $ Str xs
distributeMetaToInline (Normal xs) =
  Normal <$> mapM distributeMetaToInline xs
distributeMetaToInline (Emph xs) =
  Emph <$> mapM distributeMetaToInline xs
distributeMetaToInline Space =
  return $ Space
distributeMetaToInline (Citation cit _) =
  asks metaCiteDB >>= \ db ->
  return $ Citation cit (Just (fmtMultiCite db cit))

-- Stub. Ignores citation mode and pre-/post-notes.
fmtMultiCite :: CiteDB -> MultiCite -> [Inline]
fmtMultiCite db (MultiCite _ _ _ xs) =
  intercalate [Str ";", Space] (map (fmtSingleCite db) xs)

-- Stub. Ignores pre-/post-notes.
fmtSingleCite :: CiteDB -> SingleCite -> [Inline]
fmtSingleCite db (SingleCite _ _ keys) =
  fmtCiteEntries (mapMaybe (flip M.lookup db) keys)
