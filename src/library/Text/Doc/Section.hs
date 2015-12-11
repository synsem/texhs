----------------------------------------------------------------------
-- |
-- Module      :  Text.Doc.Section
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  GHC
--
-- Section view on document content.
----------------------------------------------------------------------

module Text.Doc.Section
  ( -- * Section-based document type
    SectionDoc(..)
  , Section(..)
    -- * Conversion
  , doc2secdoc
    -- * Accessor functions
  , sdocTitle
  , sdocAuthors
  , sdocDate
  ) where

import Text.Doc.Types


-------------------- Section type

-- | 'SectionDoc' provides a section view on 'Doc' documents:
-- The document content is split into hierarchical 'Section' elements.
data SectionDoc = SectionDoc Meta [Section]

-- | A 'Section' consists of header information (level, title) and
-- the content of the section, followed by a list of subsections.
data Section = Section Level [Inline] [Block] [Section]


-------------------- Conversion

-- | Convert a 'Doc' document to a 'SectionDoc'
-- which provides a section-based view on the content.
doc2secdoc :: Doc -> SectionDoc
doc2secdoc (Doc meta content) = SectionDoc meta (blocks2sections content)

-- Group a list of blocks into sections.
--
-- Note: Block elements that precede the first heading are dropped.
blocks2sections :: [Block] -> [Section]
blocks2sections [] = []
blocks2sections ((Header hlevel htitle):bls) =
  let (secbody, secs) = break isHeader bls
      (subsecs, aftersec) = break (isHeaderNotWithin hlevel) secs
  in (Section hlevel htitle secbody (blocks2sections subsecs)):(blocks2sections aftersec)
blocks2sections (_:bls) = blocks2sections bls -- drop leading non-headers

-- Test whether a 'Block' is a 'Header' that is not within a given level.
--
-- This is used to detect section breaks.
isHeaderNotWithin :: Level -> Block -> Bool
isHeaderNotWithin n (Header m _) = m <= n
isHeaderNotWithin _ _ = False


-------------------- Accessor functions

-- | Extract meta information.
sdocMeta :: SectionDoc -> Meta
sdocMeta (SectionDoc meta _) = meta

-- | Extract document title.
sdocTitle :: SectionDoc -> [Inline]
sdocTitle = metaTitle . sdocMeta

-- | Extract document authors.
sdocAuthors :: SectionDoc -> [[Inline]]
sdocAuthors = metaAuthors . sdocMeta

-- | Extract document date.
sdocDate :: SectionDoc -> [Inline]
sdocDate = metaDate . sdocMeta
