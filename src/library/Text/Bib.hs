----------------------------------------------------------------------
-- |
-- Module      :  Text.Bib
-- Copyright   :  2015-2016 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  GHC
--
-- Bibliography interface for parsing BibTeX files
-- and generating citations.
----------------------------------------------------------------------

module Text.Bib
  ( -- * Types
    BibDB
  , BibEntry(..)
  , Agent(..)
  , CiteKey
    -- * Reader
  , fromBibTeX
  , fromBibTeXFile
    -- * Writer
    -- ** Resolve
  , resolveCitations
    -- ** Query
  , getCiteAgents
  , getCiteYear
    -- ** Format
  , fmtCiteAgents
  , fmtCiteFull
  ) where


import Text.Bib.Types
import Text.Bib.Reader.BibTeX (fromBibTeX, fromBibTeXFile)
import Text.Bib.Writer
