{-# LANGUAGE OverloadedStrings #-}
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
    resolve
  , inheritXData
  , inheritCrossref
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Text.Bib.Types


---------- General inheritance

-- | Resolve cross-references implied by @xdata@ and @crossref@ fields.
--
-- Any @XData@ entries are dropped.
resolve :: BibDB -> BibDB
resolve db =
  M.filter ((/= "xdata") . bibType) $
  M.mapWithKey (inherit db) db

-- Resolve @xdata@ and @crossref@ inheritance for a single entry.
--
-- Note: @xdata@ references have priority over @crossref@ fields
-- and are resolved first.
inherit :: BibDB -> CiteKey -> BibEntry -> BibEntry
inherit db key = inheritCrossref db key . inheritXData db key

-- Retrieve the inheritable fields of an entry (identified by its citekey).
heritage :: BibDB -> CiteKey -> BibFieldMap
heritage db key = maybe M.empty (bibFields . inherit db key) (M.lookup key db)


---------- XData inheritance

-- | Resolve @xdata@ inheritance for a single entry.
inheritXData :: BibDB -> CiteKey -> BibEntry -> BibEntry
inheritXData db key entry =
  mergeXData (deleteField "xdata" entry)
  (map (heritage (M.delete key db)) (getXDataKeys entry))

-- Extract 'CiteKey' list from the @xdata@ field of an entry.
getXDataKeys :: BibEntry -> [CiteKey]
getXDataKeys entry =
  filter (not . T.null) . T.split (==',') $
  maybe "" id (getBibRaw "xdata" entry)

-- Merge @xdata@-inherited fields into an entry.
--
-- Own fields are preferred (i.e. never overwritten).
-- In case of duplicate inherited fields, the last value wins (right-biased union).
mergeXData :: BibEntry -> [BibFieldMap] -> BibEntry
mergeXData (BibEntry btype ownFields) inheritedFields =
  BibEntry btype (M.union ownFields (M.unionsWith (flip const) inheritedFields))


---------- Crossref inheritance

-- | Resolve @crossref@ inheritance for a single entry.
inheritCrossref :: BibDB -> CiteKey -> BibEntry -> BibEntry
inheritCrossref db key entry = maybe entry resolveEntry (getCrossrefKey entry)
  where
    resolveEntry :: CiteKey -> BibEntry
    resolveEntry = mergeCrossref entry . heritage (M.delete key db)

-- Extract a single 'CiteKey' from the @crossref@ field of an entry.
getCrossrefKey :: BibEntry -> Maybe CiteKey
getCrossrefKey = getBibRaw "crossref"

-- Merge @crossref@-inherited fields into an entry.
--
-- Private fields are not inherited and selected fields are renamed.
mergeCrossref :: BibEntry -> BibFieldMap -> BibEntry
mergeCrossref (BibEntry btype ownFields) inheritedFields =
  BibEntry btype (M.union ownFields (prepare inheritedFields))
  where
    prepare :: BibFieldMap -> BibFieldMap
    prepare = renameInheritedFields . flip (foldr M.delete) privateFields

-- Simplified field renaming scheme for @crossref@ inheritance:
-- Prepend @book@ to @author@, @title@ and @subtitle@ fields.
--
-- Note: Biber uses a more refined renaming scheme.
-- See Appendix B in the BibLaTeX manual.
--
-- Note: This will overwrite existing fields with the value
-- of the renamed field (in line with biber-2.2). For example,
-- the value of any pre-existing @booktitle@ field will be
-- overwritten by the value of the @title@ field (if it exists).
renameInheritedFields :: BibFieldMap -> BibFieldMap
renameInheritedFields = M.mapKeys rename
  where
    rename :: BibFieldName -> BibFieldName
    rename n
      | n `elem` renamedFields = T.append "book" n
      | otherwise = n
    renamedFields :: [BibFieldName]
    renamedFields = ["author", "title", "subtitle"]

-- List of private fields that are not inherited.
--
-- See Appendix B in the BibLaTeX manual.
privateFields :: [BibFieldName]
privateFields =
  [ "ids", "crossref", "xref", "label", "options"
  , "presort", "sortkey", "entryset", "entrysubset"
  ]
