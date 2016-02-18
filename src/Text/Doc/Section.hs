----------------------------------------------------------------------
-- |
-- Module      :  Text.Doc.Section
-- Copyright   :  2015-2016 Mathias Schenner,
--                2015-2016 Language Science Press.
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
  , blocks2sections
  , splitRegions
  ) where

import Text.Doc.Types


-------------------- Section type

-- | 'SectionDoc' provides a section view on 'Doc' documents:
-- The document content is split into hierarchical 'Section' elements.
data SectionDoc = SectionDoc Meta [Section]
  deriving (Eq, Show)

instance HasMeta SectionDoc where
  docMeta (SectionDoc meta _) = meta

-- | A 'Section' consists of header information (level, title) and
-- the content of the section, followed by a list of subsections.
data Section = Section Level InternalAnchor [Inline] [Block] [Section]
  deriving (Eq, Show)


-------------------- Conversion

-- | Convert a 'Doc' document to a 'SectionDoc'
-- which provides a section-based view on the content.
doc2secdoc :: Doc -> SectionDoc
doc2secdoc (Doc meta content) = SectionDoc meta (blocks2sections content)

-- | Group a list of blocks into sections.
--
-- Note: Block elements that precede the first heading are dropped.
blocks2sections :: [Block] -> [Section]
blocks2sections [] = []
blocks2sections (Header hlevel hanchor htitle : bls) =
  let (secbody, secs) = break isHeader bls
      (subsecs, aftersec) = break (isHeaderNotWithin hlevel) secs
  in Section hlevel hanchor htitle secbody (blocks2sections subsecs) : blocks2sections aftersec
blocks2sections (_:bls) = blocks2sections bls -- drop leading non-headers

-- Test whether a 'Block' is a 'Header' that is not within a given level.
--
-- This is used to detect section breaks.
isHeaderNotWithin :: Level -> Block -> Bool
isHeaderNotWithin n (Header m _ _) = m <= n
isHeaderNotWithin _ _ = False

-- | Split a list of sections into three book regions:
-- Frontmatter, Mainmatter, and Backmatter.
--
-- This function only looks at top-level sections and assumes that
-- all Frontmatter sections precede any Mainmatter sections and
-- that all Mainmatter sections precede any Backmatter sections.
splitRegions :: [Section] -> ([Section], [Section], [Section])
splitRegions xs =
  let (frontMatter, ys) = span isFrontmatter xs
      (mainMatter, backMatter) = span isMainmatter ys
  in (frontMatter, mainMatter, backMatter)

-- Test whether the section is part of the frontmatter.
isFrontmatter :: Section -> Bool
isFrontmatter (Section _ (SectionAnchor (SectionInfo Frontmatter _)) _ _ _) = True
isFrontmatter _ = False

-- Test whether the section is part of the mainmatter.
isMainmatter :: Section -> Bool
isMainmatter (Section _ (SectionAnchor (SectionInfo Mainmatter _)) _ _ _) = True
isMainmatter _ = False
