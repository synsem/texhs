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
   docBibFilter
 ) where

import Text.Bib (BibDB, resolveCitations)
import Text.Doc.Types


-------------------- Bib Filter

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
