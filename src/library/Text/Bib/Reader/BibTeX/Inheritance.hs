----------------------------------------------------------------------
-- |
-- Module      :  Text.Bib.Reader.BibTeX.Inheritance
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  portable
--
-- BibTeX parser, layer 3:
-- resolve cross-references between BibTeX entries
-- (@crossref@ and @xdata@ fields).
----------------------------------------------------------------------

module Text.Bib.Reader.BibTeX.Inheritance
  ( -- * Resolve cross-references
    resolveCrossrefs
  ) where

import Text.Bib.Types


-- Stub.
-- | Inherit fields via @xdata@ and @crossref@ references.
--
-- Note: @xdata@ references have priority over @crossref@ fields
-- and are resolved first.
resolveCrossrefs :: BibDB -> BibDB
resolveCrossrefs = id
