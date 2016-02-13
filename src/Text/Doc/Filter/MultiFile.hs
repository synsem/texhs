----------------------------------------------------------------------
-- |
-- Module      :  Text.Doc.Filter.MultiFile
-- Copyright   :  2015-2016 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  GHC
--
-- Split a document into multiple physical files.
----------------------------------------------------------------------

module Text.Doc.Filter.MultiFile
 ( -- * Types
   ContentFile(..)
 , FileMap
   -- * Document split
 , splitDoc
 , mkAnchorFileMap
   -- * Anchor extraction
 , extractAnchors
 ) where


import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Text.Doc.Section
import Text.Doc.Types


-------------------- Types

-- | A ContentFile contains a potentially incomplete section
-- (in particular, its subsections may be missing)
-- that is intended to be written into a file on its own.
newtype ContentFile = ContentFile Section
  deriving (Eq, Show)

-- | A collection of ContentFiles,
-- indexed by their FileID.
type FileMap = Map FileID ContentFile


-------------------- Document split

-- | Split a document into ContentFiles
-- at the indicated section level.
splitDoc :: Level -> SectionDoc -> FileMap
splitDoc splitLevel (SectionDoc _ content) =
  M.fromList $ zip [1..] $
  concatMap (splitSection splitLevel) content

-- | Split a single section into ContentFiles
-- at the indicated section level.
splitSection :: Level -> Section -> [ContentFile]
splitSection splitLevel sec@(Section level anchor title content subsecs)
  | level >= splitLevel = [ContentFile sec]
  | otherwise = ContentFile (Section level anchor title content []) :
                concatMap (splitSection splitLevel) subsecs

-- | Create a map for looking up in which file
-- a given anchor is located.
mkAnchorFileMap :: FileMap -> AnchorFileMap
mkAnchorFileMap fileMap =
  let fileAnchorMap = M.map extractAnchors fileMap
      insertAnchors fileID anchors m = foldr (`M.insert` fileID) m anchors
  in M.foldrWithKey' insertAnchors M.empty fileAnchorMap


-------------------- Anchor extraction

-- | Extract internal anchors from a ContentFile.
extractAnchors :: ContentFile -> [InternalAnchor]
extractAnchors (ContentFile s) = extractAnchorsFromSection s

-- Extract internal anchors from a Section.
extractAnchorsFromSection :: Section -> [InternalAnchor]
extractAnchorsFromSection (Section _ anchor title content subsecs) =
  anchor : extractAnchorsFromInlines title ++
  extractAnchorsFromBlocks content ++
  concatMap extractAnchorsFromSection subsecs

-- Extract internal anchors from a list of blocks.
extractAnchorsFromBlocks :: [Block] -> [InternalAnchor]
extractAnchorsFromBlocks = concatMap extractAnchorsFromBlock

-- Extract internal anchors from a list of inlines.
extractAnchorsFromInlines :: [Inline] -> [InternalAnchor]
extractAnchorsFromInlines = concatMap extractAnchorsFromInline

-- Extract internal anchors from a Block.
extractAnchorsFromBlock :: Block -> [InternalAnchor]
extractAnchorsFromBlock (Para is) =
  extractAnchorsFromInlines is
extractAnchorsFromBlock (Header _ anchor is) =
  anchor : extractAnchorsFromInlines is
extractAnchorsFromBlock (List _ bss) =
  concatMap extractAnchorsFromBlocks bss
extractAnchorsFromBlock (ListItemBlock ls) =
  concatMap extractAnchorsFromListItem ls
extractAnchorsFromBlock (QuotationBlock bs) =
  extractAnchorsFromBlocks bs
extractAnchorsFromBlock (Figure anchor _ is) =
  anchor : extractAnchorsFromInlines is
extractAnchorsFromBlock (Table anchor is cs) =
  anchor : extractAnchorsFromInlines is ++
  concatMap (concatMap extractAnchorsFromTableCell) cs
extractAnchorsFromBlock (SimpleTable cs) =
  concatMap (concatMap extractAnchorsFromTableCell) cs

-- Extract internal anchors from a ListItem.
extractAnchorsFromListItem :: ListItem -> [InternalAnchor]
extractAnchorsFromListItem (ListItem anchor bs) =
  anchor : extractAnchorsFromBlocks bs

-- Extract internal anchors from a TableCell.
extractAnchorsFromTableCell :: TableCell -> [InternalAnchor]
extractAnchorsFromTableCell (SingleCell is) =
  extractAnchorsFromInlines is
extractAnchorsFromTableCell (MultiCell _ is) =
  extractAnchorsFromInlines is

-- Extract internal anchors from an Inline.
extractAnchorsFromInline :: Inline -> [InternalAnchor]
extractAnchorsFromInline Str{} = []
extractAnchorsFromInline (FontStyle _ is) =
  extractAnchorsFromInlines is
extractAnchorsFromInline (Math _ is) =
  extractAnchorsFromInlines is
extractAnchorsFromInline Space = []
extractAnchorsFromInline (Citation (MultiCite _ is1 is2 cits)) =
  extractAnchorsFromInlines is1 ++
  extractAnchorsFromInlines is2 ++
  concatMap extractAnchorsFromSingleCite cits
extractAnchorsFromInline Pointer{} = []
extractAnchorsFromInline (Note anchor bs) =
  anchor : extractAnchorsFromBlocks bs

-- Extract internal anchors from a SingleCite.
extractAnchorsFromSingleCite :: SingleCite -> [InternalAnchor]
extractAnchorsFromSingleCite (SingleCite is1 is2 _) =
  extractAnchorsFromInlines is1 ++ extractAnchorsFromInlines is2
