----------------------------------------------------------------------
-- |
-- Module      :  Text.Bib
-- Copyright   :  2015-2017 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
--
-- Maintainer  :  mschenner.dev@gmail.com
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
    -- ** Format
  , fmtCiteAgents
  , fmtCiteYear
  , fmtCiteFull
  ) where


import Text.Bib.Types
import Text.Bib.Reader.BibTeX (fromBibTeX, fromBibTeXFile)
import Text.Bib.Writer
