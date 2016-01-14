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
  , Location
  , Anchor(..)
  , InternalAnchor(..)
  , anchorID
  , anchorTarget
  , anchorDescription
  , registerAnchorLabel
  , incSection
  , getChapter
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

import Data.List (dropWhileEnd, intersperse, intercalate)
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
  , metaAnchorCurrent :: InternalAnchor
  , metaAnchorMap :: Map Label InternalAnchor
  , metaSectionCurrent :: [Int]
  , metaFigureCurrent :: (Int, Int)
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
  , metaFigureCurrent = (0, 0)
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
data Anchor
  = InternalResource InternalAnchor
  | ExternalResource [Inline] Location
  deriving (Eq, Show)

-- | An internal anchor is a location within the current document.
--
-- Internal anchors and pointers are used to model cross-references.
data InternalAnchor
  = DocumentAnchor          -- initial anchor in a document
  | SectionAnchor [Int]     -- section numbers
  | FigureAnchor (Int, Int) -- chapter number and chapter-relative figure count
  deriving (Eq, Show)

-- | Generate identifying string for an anchor.
--
-- This can be used for @xml:id@ attributes.
anchorID :: InternalAnchor -> Text
anchorID DocumentAnchor = ""
anchorID (SectionAnchor xs) = T.concat $ zipWith T.append
  ["Pt", "Ch", "S", "s", "ss", "p"]
  (map (T.pack . show) xs)
anchorID (FigureAnchor (chap, fignum)) = T.concat $ zipWith T.append
  ["figure", "chap"]
  (map (T.pack . show) [fignum, chap])

-- | Generate target location string for an anchor.
--
-- This can be used for @href@ attributes in HTML hyperlinks.
anchorTarget :: Anchor -> Text
anchorTarget (InternalResource i) = T.cons '#' (anchorID i)
anchorTarget (ExternalResource _ loc) = loc

-- | Generate description text for an anchor.
anchorDescription :: Anchor -> [Inline]
anchorDescription (InternalResource i) = internalAnchorDescription i
anchorDescription (ExternalResource l _) = l

-- Generate description text for an internal anchor.
--
-- Helper for 'anchorDescription'.
internalAnchorDescription :: InternalAnchor -> [Inline]
internalAnchorDescription DocumentAnchor = [Str "start"]
internalAnchorDescription (SectionAnchor xs) =
  [Str (intercalate "." (map show xs))]
internalAnchorDescription (FigureAnchor (chap, fignum)) =
  [Str (intercalate "." (map show [chap, fignum]))]

-- | A label is a name of an internal anchor.
--
-- In particular, it can be used by a 'Pointer' as
-- a way to refer to an 'InternalAnchor' in the document.
type Label = Text

-- | A description of the location of a resource, typically an URI.
--
-- This is used as the link target of an 'ExternalResource'.
type Location = Text

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

-- | Extract chapter number from section cursor.
getChapter :: [Int] -> Int
getChapter sec
  | length sec >= 2 = sec !! 1
  | otherwise = 0

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
  | Header Level InternalAnchor [Inline]
  | List ListType [[Block]]
  | QuotationBlock [Block]
  | Figure InternalAnchor Location [Inline]
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
