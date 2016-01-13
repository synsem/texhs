{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.Doc.Types
-- Copyright   :  2015-2016 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  GHC
--
-- The Doc type and utility functions.
----------------------------------------------------------------------

module Text.Doc.Types
  ( -- * Doc type
    Doc(..)
    -- ** Meta
  , Meta(..)
  , defaultMeta
  , HasMeta(..)
  , CiteKey
  , CiteDB
  , CiteEntry(..)
  , registerCiteKeys
  , Label
  , Anchor(..)
  , anchorURI
  , anchorDescription
  , registerAnchorLabel
  , incSection
    -- ** Blocks
  , Content
  , Level
  , ListType(..)
  , Block(..)
    -- ** Inlines
  , Inline(..)
  , MultiCite(..)
  , SingleCite(..)
  , CiteMode(..)
    -- * Block predicates
  , isPara
  , isHeader
  , isList
    -- * Inline predicates
  , isStr
  , isNormal
  , isEmph
  , isSpace
    -- * Accessor functions
  , docTitle
  , docAuthors
  , docDate
  , plain
    -- * Normalization
  , normalizeInlines
  , stripInlines
  ) where

import Data.List (dropWhileEnd, intersperse)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T


-------------------- Doc type

-- | A 'Doc' document consist of meta information and content.
data Doc = Doc Meta Content
  deriving (Eq, Show)


-------------------- Meta type

-- | Meta information of a 'Doc' document.
data Meta = Meta
  { metaTitle :: [Inline]
  , metaAuthors :: [[Inline]]
  , metaDate :: [Inline]
  , metaCiteDB :: CiteDB
  , metaCiteMap :: Map CiteKey Int
  , metaCiteCount :: Int
  , metaAnchorCurrent :: Anchor
  , metaAnchorMap :: Map Label Anchor
  , metaSectionCurrent :: [Int]
  } deriving (Eq, Show)

-- | Default (empty) meta information of a document.
defaultMeta :: Meta
defaultMeta = Meta
  { metaTitle = []
  , metaAuthors = []
  , metaDate = []
  , metaCiteDB = M.empty
  , metaCiteMap = M.empty
  , metaCiteCount = 0
  , metaAnchorCurrent = DocumentAnchor
  , metaAnchorMap = M.empty
  , metaSectionCurrent = replicate 6 0
  }

-- | A class for document types that hold meta information.
class HasMeta d where
  -- | Extract meta information.
  docMeta :: d -> Meta

instance HasMeta Doc where
  docMeta (Doc meta _) = meta

---------- Cross-references

-- | An anchor is a location in a document
-- that can be the target of a 'Pointer'.
-- Anchors and pointers are used to model cross-references.
data Anchor
  = DocumentAnchor -- initial anchor in a document
  | SectionAnchor [Int]
  deriving (Eq, Show)

-- | Generate identifying string for an anchor.
anchorURI :: Anchor -> Text
anchorURI DocumentAnchor = ""
anchorURI (SectionAnchor xs) = T.concat $ zipWith T.append
  ["Pt", "Ch", "S", "s", "ss", "p"]
  (map (T.pack . show) xs)

-- | Generate description text for an anchor.
anchorDescription :: Anchor -> Text
anchorDescription DocumentAnchor = "start"
anchorDescription (SectionAnchor xs) = T.intercalate "." (map (T.pack . show) xs)

-- | A label is a name of an anchor.
--
-- In particular, it can be used by a 'Pointer' as
-- a way to refer to an 'Anchor' in the document.
type Label = Text

-- | Assign a label to the current anchor
-- in the document meta information.
registerAnchorLabel :: Label -> Meta -> Meta
registerAnchorLabel label meta =
  let anchor = metaAnchorCurrent meta
      newMap = M.insert label anchor (metaAnchorMap meta)
  in meta { metaAnchorMap = newMap }

---------- Citations

-- | Identifier key for bibliographic entries.
type CiteKey = Text

-- | A collection of formatted citations.
type CiteDB = Map CiteKey CiteEntry

-- | Citation information for a single bibliographic entry.
--
-- This can be used to generate an author-year style citation
-- and a full bibliographic reference for a single entry.
data CiteEntry = CiteEntry
  { citeAgents :: [[Inline]]
  , citeYear   :: [Inline]
  , citeFull   :: [Inline]
  } deriving (Eq, Show)

-- | Add a list of citekeys to document meta information.
registerCiteKeys :: [CiteKey] -> Meta -> Meta
registerCiteKeys keys meta =
  let citeCount = metaCiteCount meta
      citeMap = metaCiteMap meta
      keysNum = zip keys [citeCount..]
      insertIfNew = uncurry (M.insertWith (flip const))
      newCount = citeCount + length keys
      newMap = foldr insertIfNew citeMap keysNum
  in meta { metaCiteMap = newMap
          , metaCiteCount = newCount
          }

---------- Sections

-- | Increment the current section at the given level.
incSection :: Level -> [Int] -> [Int]
incSection n c = uncurry (++) $ fmap incLevels $ splitAt (n-1) c
  where
    incLevels [] = []
    incLevels (x:xs) = x+1 : replicate (length xs) 0

-------------------- Content type

-- | Content of a 'Doc' document.
type Content = [Block]

-- | Level of a heading.
type Level = Int

-- | Type of list: unordered or ordered.
data ListType = UnorderedList | OrderedList
  deriving (Eq, Show)

-- | Block elements in a 'Doc' document.
data Block
  = Para [Inline]
  | Header Level Anchor [Inline]
  | List ListType [[Block]]
  deriving (Eq, Show)

-- | Inline elements in a 'Doc' document.
data Inline
  = Str String
  | Normal [Inline]
  | Emph [Inline]
  | Space
  | Citation MultiCite (Maybe [Inline])
  | Pointer Label (Maybe Anchor)
  deriving (Eq, Show)

-- | A list of 'SingleCite' citations
-- with optional prenote and postnote.
--
-- This is modeled after biblatex's
-- multicite commands (e.g. @\\cites@).
data MultiCite = MultiCite
  { multiCiteMode :: CiteMode
  , multiCitePrenote :: [Inline]
  , multiCitePostnote :: [Inline]
  , multiCites :: [SingleCite]
  } deriving (Eq, Show)

-- | A single citation
-- comprised of a list of citekeys
-- and optional prenote and postnote.
--
-- This is modeled after biblatex's
-- basic cite commands (e.g. @\\cite@).
data SingleCite = SingleCite
  { singleCitePrenote :: [Inline]
  , singleCitePostnote :: [Inline]
  , singleCiteKeys :: [CiteKey]
  } deriving (Eq, Show)

-- | The type of a citation.
--
-- This may be used by document writers to adjust
-- the formatting of a citation.
data CiteMode
  = CiteParen   -- like biblatex's @\\parencite@, aka @\\citep@
  | CiteText    -- like biblatex's @\\textcite@, aka @\\citet@
  deriving (Eq, Show)


-------------------- Block predicates

-- | Test whether a 'Block' is a 'Para'.
isPara :: Block -> Bool
isPara Para{} = True
isPara _ = False

-- | Test whether a 'Block' is a 'Header'.
isHeader :: Block -> Bool
isHeader Header{} = True
isHeader _ = False

-- | Test whether a 'Block' is a 'List'.
isList :: Block -> Bool
isList List{} = True
isList _ = False


-------------------- Inline predicates

-- | Test whether an 'Inline' is a 'Str'.
isStr :: Inline -> Bool
isStr Str{} = True
isStr _ = False

-- | Test whether an 'Inline' is a 'Normal'.
isNormal :: Inline -> Bool
isNormal Normal{} = True
isNormal _ = False

-- | Test whether an 'Inline' is an 'Emph'.
isEmph :: Inline -> Bool
isEmph Emph{} = True
isEmph _ = False

-- | Test whether an 'Inline' is a 'Space'.
isSpace :: Inline -> Bool
isSpace Space = True
isSpace _ = False


-------------------- Accessor functions

-- | Extract document title.
docTitle :: HasMeta d => d -> [Inline]
docTitle = metaTitle . docMeta

-- | Extract document authors.
docAuthors :: HasMeta d => d -> [[Inline]]
docAuthors = metaAuthors . docMeta

-- | Extract document date.
docDate :: HasMeta d => d -> [Inline]
docDate = metaDate . docMeta

-- Warning: The citation formatter drops any prenotes and postnotes.
-- | Extract plain character data from an 'Inline' element.
plain :: Inline -> String
plain (Str xs) = xs
plain (Normal is) = concatMap plain is
plain (Emph is) = concatMap plain is
plain Space = " "
plain (Citation (MultiCite _ _ _ xs) _) = T.unpack (plainCites xs)
plain (Pointer label _) = T.unpack (T.concat ["<", label, ">"])

-- Helper for showing raw citations.
plainCites :: [SingleCite] -> Text
plainCites = T.intercalate ";" . map plainCite

-- Helper for showing raw citations.
plainCite :: SingleCite -> Text
plainCite (SingleCite _ _ sKeys) = T.concat (intersperse "," sKeys)


-------------------- Normalization

-- | Conflate adjacent strings.
normalizeInlines :: [Inline] -> [Inline]
normalizeInlines [] = []
normalizeInlines (Str xs : Str ys : zs) = normalizeInlines (Str (xs ++ ys) : zs)
normalizeInlines (xs : ys) = xs : normalizeInlines ys

-- | Strip leading and trailing whitespace.
stripInlines :: [Inline] -> [Inline]
stripInlines = dropWhile isSpace . dropWhileEnd isSpace
