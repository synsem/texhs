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
  , fmtExtraYear
  ) where

import Control.Applicative
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Tuple (swap)

import Text.Bib.Types
import Text.Doc.Types


-------------------- Resolve

-- | Create a collection of formatted citations ('CiteEntry')
-- based on a set of citekeys (from 'CiteMap') and an underlying
-- bibliographic database ('BibDB').
resolveCitations :: BibDB -> CiteMap -> CiteDB
resolveCitations db citeMap =
  let uniqMap = mkCiteUniqueMap db citeMap
      resolve = resolveCitation db citeMap uniqMap
  in M.fromList $ mapMaybe resolve (M.keys citeMap)

-- Create a formatted citation ('CiteEntry') for a given citekey.
resolveCitation :: BibDB -> CiteMap -> Map CiteKey Int
                   -> CiteKey -> Maybe (CiteKey, CiteEntry)
resolveCitation bibdb citeMap uniqMap key =
  let uniqcite = M.lookup key uniqMap
      bibAnchor = BibAnchor <$> M.lookup key citeMap
  in (,) key <$> (mkCiteEntry uniqcite <$> bibAnchor <*> M.lookup key bibdb)

-- | Prepare author-year disambiguation information for every citekey.
--
-- Note: 'M.lookup' on this map returns a valid 'CiteUnique' value
-- (since unambiguous keys are not stored in this map).
mkCiteUniqueMap :: BibDB -> CiteMap -> Map CiteKey Int
mkCiteUniqueMap db citeMap =
  let keyEntryPairs :: [(CiteKey, BibEntry)]
      keyEntryPairs = mapMaybe -- drop unknown keys
        (\ key -> (,) key <$> M.lookup key db)
        (M.keys citeMap)
      -- Note: We cannot use (agents, year) in the form of ([[Inline]], [Inline])
      -- as proper keys in a Map (unless we add an 'Ord' instance), so we fall
      -- back to a simple ([String], String) representation (via 'plain') for now.
      mkNameYearHash :: BibEntry -> ([String], String)
      mkNameYearHash e = ( map (concatMap plain) (getCiteAgents e)
                         , concatMap plain (getCiteYear e))
      inverseMap :: Map ([String], String) [CiteKey]
      inverseMap = foldr
        (uncurry (M.insertWith (++)) . fmap (:[]) . swap . fmap mkNameYearHash)
        M.empty
        keyEntryPairs
  in M.foldr (M.union . M.fromList . toCiteUnique) M.empty inverseMap

-- Attach disambiguation label to a list of citekeys
-- with identical author-year key. Empty and singleton
-- lists need no disambiguation, so they are dropped.
toCiteUnique :: [CiteKey] -> [(CiteKey, Int)]
toCiteUnique [] = []
toCiteUnique [_] = []
toCiteUnique ks@(_:_) = zip ks [0..]


-------------------- Query

-- Extract citation information from a bib entry.
mkCiteEntry :: CiteUnique -> InternalAnchor -> BibEntry -> CiteEntry
mkCiteEntry uniqcite anchor entry = CiteEntry
  (getCiteAgents entry)
  (getCiteYear entry)
  uniqcite
  (fmtCiteFull entry)
  anchor

-- | Retrieve a list of last names of authors or editors
-- for an author-year citation.
getCiteAgents :: BibEntry -> [[Inline]]
getCiteAgents entry =
  maybe [] (map agentLast)
    (getBibAgents "author" entry <|>
     getBibAgents "editor" entry)

-- | Construct year part of an author-year citation.
getCiteYear :: BibEntry -> [Inline]
getCiteYear = fromMaybe [] . getBibLiteral "year"


-------------------- Format

-- | Construct author-year citation for a list of cite entries.
fmtCiteEntries :: [CiteEntry] -> [Inline]
fmtCiteEntries = intercalate [Str ";", Space] . map fmtCiteEntry

-- | Construct author-year citation for a single cite entry.
fmtCiteEntry :: CiteEntry -> [Inline]
fmtCiteEntry (CiteEntry authors year _ _ _) =
  fmtCiteAgents authors ++ [Space] ++ year

-- | Construct author part of an author-year citation
-- from a list of last names of authors.
fmtCiteAgents :: [[Inline]] -> [Inline]
fmtCiteAgents authors =
  let nrAuthors = length authors
      sepInner = [Str ",", Space]
      sepFinal = [Space, Str "&", Space]
      sepInners = replicate (max 0 (nrAuthors - 2)) sepInner
      sepFinals = if nrAuthors > 1 then [sepFinal] else [[]]
      fillers = sepInners ++ sepFinals
  in concat $ zipWith (++) authors fillers

-- | Construct full bibliographic reference for an entry.
fmtCiteFull :: BibEntry -> [Inline]
fmtCiteFull entry =
  fmtCiteAgents (getCiteAgents entry) ++ [Space] ++
  getCiteYear entry ++ [Space] ++
  fromMaybe [] (getBibLiteral "title" entry) ++
  [Str "."]

-- | Convert a 'CiteUnique' value to a textual @extrayear@ suffix
-- that can be appended to the year part in author-year citations.
-- For example, @Just 0@ is converted to @\"a\"@, as in \"2000a\".
fmtExtraYear :: CiteUnique -> String
fmtExtraYear Nothing = ""
fmtExtraYear (Just n) = case n `divMod` 26 of
  (q,r) | q < 0 -> "" -- error: invalid negative value (ignore)
        | q == 0 -> [['a'..'z'] !! r]
        | otherwise -> fmtExtraYear (Just (q-1)) ++ [['a'..'z'] !! r]
