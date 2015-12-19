----------------------------------------------------------------------
-- |
-- Module      :  Text.Doc.Filter.Bib
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
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
   docBibFilter
 ) where

import qualified Data.Map.Strict as M

import Text.Bib.Types (BibDB)
import Text.Bib.Writer (resolveCitations)
import Text.Doc.Types


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
