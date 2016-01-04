{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.Bib.Writer
-- Copyright   :  2015-2016 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  GHC
--
-- BibTeX formatter.
----------------------------------------------------------------------

module Text.Bib.Writer
  ( -- * Resolve
    resolveCitations
    -- * Query
  , getCiteAgents
  , getCiteYear
    -- * Format
  , fmtCiteEntries
  , fmtCiteEntry
  , fmtCiteAgents
  , fmtCiteFull
  ) where

import Control.Applicative
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)

import Text.Bib.Types
import Text.Doc.Types


-------------------- Resolve

-- Note: Citation ambiguity detection is not yet implemented.
-- | Create a collection of formatted citations
-- based on a set of citekeys and an underlying bibliographic database.
resolveCitations :: BibDB -> [CiteKey] -> CiteDB
resolveCitations db keys =
  M.fromList $ mapMaybe (resolveCitation db) keys

-- Create a formatted citation for a given citekey.
resolveCitation :: BibDB -> CiteKey -> Maybe (CiteKey, CiteEntry)
resolveCitation db key = (,) key . mkCiteEntry <$> M.lookup key db


-------------------- Query

-- Extract citation information from a bib entry.
mkCiteEntry :: BibEntry -> CiteEntry
mkCiteEntry e = CiteEntry
  (getCiteAgents e)
  (getCiteYear e)
  (fmtCiteFull e)

-- | Retrieve a list of last names of authors or editors
-- for an author-year citation.
getCiteAgents :: BibEntry -> [[Inline]]
getCiteAgents entry =
  maybe [] (map agentLast)
    (getBibAgents "author" entry <|>
     getBibAgents "editor" entry)

-- | Construct year part of an author-year citation.
getCiteYear :: BibEntry -> [Inline]
getCiteYear = maybe [] id . getBibLiteral "year"


-------------------- Format

-- | Construct author-year citation for a list of cite entries.
fmtCiteEntries :: [CiteEntry] -> [Inline]
fmtCiteEntries = intercalate [Str ";", Space] . map fmtCiteEntry

-- | Construct author-year citation for a single cite entry.
fmtCiteEntry :: CiteEntry -> [Inline]
fmtCiteEntry (CiteEntry authors year _) =
  fmtCiteAgents authors ++ [Space] ++ year

-- | Construct author part of an author-year citation
-- from a list of last names of authors.
fmtCiteAgents :: [[Inline]] -> [Inline]
fmtCiteAgents authors =
  let nrAuthors = length authors
      sepInner = [Str ",", Space]
      sepFinal = [Space, Str "&", Space]
      sepInners = replicate (max 0 (nrAuthors - 2)) sepInner
      sepFinals = if nrAuthors > 1 then (sepFinal:[]) else [[]]
      fillers = sepInners ++ sepFinals
  in concat $ zipWith (++) authors fillers

-- | Construct full bibliographic reference for an entry.
fmtCiteFull :: BibEntry -> [Inline]
fmtCiteFull entry =
  fmtCiteAgents (getCiteAgents entry) ++ [Space] ++
  getCiteYear entry ++ [Space] ++
  maybe [] id (getBibLiteral "title" entry) ++
  [Str "."]
