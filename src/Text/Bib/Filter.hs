{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.Bib.Filter
-- Copyright   :  2015-2017 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
--
-- Maintainer  :  mschenner.dev@gmail.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- Filters for normalizing bibliographic information.
----------------------------------------------------------------------

module Text.Bib.Filter
  ( -- * Normalize
    normalizeBibLaTeX
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

import Text.Bib.Types


-------------------- Normalize

-- | Normalize BibTeX data to BibLaTeX model.
--
-- This filter should be applied to bibliographic data read from
-- legacy BibTeX files. It will transform entry and field names
-- to the model that BibLaTeX expects.
--
-- For example, \"book\" entries that have an editor but no author
-- are converted to \"collection\" entries. Similarly, some fields
-- are renamed, e.g. \"journal\" is renamed to \"journaltitle\".
normalizeBibLaTeX :: BibDB -> BibDB
normalizeBibLaTeX = M.map normalizeBibLaTeXEntry

-- Normalize a single entry to BibLaTeX model.
--
-- Note: The entry types \"phdthesis\" and \"mastersthesis\"
-- are not renamed to \"thesis\" (yet).
--
-- See Appendix in BibLaTeX manual for more details.
normalizeBibLaTeXEntry :: BibEntry -> BibEntry
normalizeBibLaTeXEntry (BibEntry btype bfields)
  | btype == "book" &&
    "editor" `M.member` bfields &&
    "author" `M.notMember` bfields =
    BibEntry "collection" (normalizeFieldNames bfields)
  | otherwise = BibEntry btype (normalizeFieldNames bfields)

-- Normalize field names of a single entry to BibLaTeX model.
normalizeFieldNames :: BibFieldMap -> BibFieldMap
normalizeFieldNames = M.mapKeys $ \k ->
  fromMaybe k (M.lookup k fieldNameMap)

-- Data map for normalizing field names.
--
-- See Appendix in BibLaTeX manual for more details.
fieldNameMap :: Map BibFieldName BibFieldName
fieldNameMap = M.fromList
  [ ("journal", "journaltitle")
  , ("address", "location")
  , ("school", "institution")
  ]
