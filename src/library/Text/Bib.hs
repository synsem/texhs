----------------------------------------------------------------------
-- |
-- Module      :  Text.Bib
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  portable
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
    -- * Writer
  , getCiteAgent
  , getCiteYear
  , getCiteFull
  ) where

import Text.Bib.Types
import Text.Bib.Reader.BibTeX (fromBibTeX)
import Text.Bib.Writer (getCiteAgent, getCiteYear, getCiteFull)
