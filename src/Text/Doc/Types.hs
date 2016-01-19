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
  , anchorTarget
  , anchorDescription
  , InternalAnchor(..)
  , internalAnchorID
  , internalAnchorIDRef
  , internalAnchorTarget
  , internalAnchorTargetRef
  , internalAnchorDescription
  , internalAnchorDescriptionAsText
  , internalAnchorLocalNum
  , registerAnchorLabel
  , incSection
  , getChapter
    -- ** Blocks
  , Content
  , Level
  , ListType(..)
  , ListItem(..)
  , TableRow
  , TableCell(..)
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
  , metaNoteMap :: Map (Int, Int) [Block]
  , metaSectionCurrent :: [Int]
  , metaFigureCurrent :: (Int, Int)
  , metaTableCurrent :: (Int, Int)
  , metaNoteCurrent :: (Int, Int)
  , metaItemCurrent :: (Int, [Int])
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
  , metaNoteMap = M.empty
  , metaSectionCurrent = replicate 6 0
  , metaFigureCurrent = (0, 0)
  , metaTableCurrent = (0, 0)
  , metaNoteCurrent = (0, 0)
  , metaItemCurrent = (0, [0])
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
  | SectionAnchor [Int]     -- section numbers: [part, chap, sect, ...]
  | FigureAnchor (Int, Int) -- chapter number and chapter-relative figure count
  | TableAnchor (Int, Int)  -- chapter number and chapter-relative table count
  | NoteAnchor (Int, Int)   -- chapter number and chapter-relative note count
  | ItemAnchor (Int, [Int]) -- chapter number and chapter-relative item numbers
                            --   in reverse order, e.g. [1,3,2] --> \"2.3.1\"
  deriving (Eq, Show)

-- | Generate identifying string for an internal anchor.
--
-- This can be used for @xml:id@ attributes.
internalAnchorID :: InternalAnchor -> Text
internalAnchorID DocumentAnchor = ""
internalAnchorID (SectionAnchor []) =
  error "internalAnchorID: invalid (empty) section numbers"
internalAnchorID (SectionAnchor [_]) =
  error "internalAnchorID: invalid (singleton) section numbers"
internalAnchorID (SectionAnchor (_:ch:ns)) =
  let trimsection = dropWhileEnd (==0)
  in T.append "sec-" (hyphenSep (ch:trimsection ns))
internalAnchorID (FigureAnchor (ch, n)) =
  T.append "figure-" (hyphenSep [ch, n])
internalAnchorID (TableAnchor (ch, n)) =
  T.append "table-" (hyphenSep [ch, n])
internalAnchorID (NoteAnchor (ch, n)) =
  T.append "note-" (hyphenSep [ch, n])
internalAnchorID (ItemAnchor (ch, ns)) =
  T.append "item-" (hyphenSep (ch:reverse ns))

-- | Generate identifying string for a reference to an anchor.
--
-- This is mainly used for footnote back-references: The footnote text
-- (main anchor) contains a back-reference to the footnote mark.
internalAnchorIDRef :: InternalAnchor -> Text
internalAnchorIDRef anchor = T.append (internalAnchorID anchor) "-ref"

-- | Generate target location string for an anchor.
--
-- This can be used for @href@ attributes in HTML hyperlinks.
anchorTarget :: Anchor -> Text
anchorTarget (InternalResource res) = internalAnchorTarget res
anchorTarget (ExternalResource _ loc) = loc

-- | Generate target location string for an internal anchor.
--
-- This can be used for @href@ attributes in HTML hyperlinks.
internalAnchorTarget :: InternalAnchor -> Text
internalAnchorTarget = T.cons '#' . internalAnchorID

-- | Generate target location string for a reference to an internal anchor.
--
-- This is mainly used for footnote back-references: The footnote text
-- (main anchor) contains a back-reference to the footnote mark.
internalAnchorTargetRef :: InternalAnchor -> Text
internalAnchorTargetRef = T.cons '#' . internalAnchorIDRef

-- | Generate description text for an anchor.
anchorDescription :: Anchor -> [Inline]
anchorDescription (InternalResource i) = internalAnchorDescription i
anchorDescription (ExternalResource l _) = l

-- | Generate description text for an internal anchor.
--
-- Intended to be used in running 'Inline' text.
internalAnchorDescription :: InternalAnchor -> [Inline]
internalAnchorDescription = (:[]) . Str . internalAnchorDescriptionAsText

-- | Generate raw description text for an internal anchor.
--
-- Intended to be used in XML attribute values.
internalAnchorDescriptionAsText :: InternalAnchor -> String
internalAnchorDescriptionAsText DocumentAnchor = "start"
internalAnchorDescriptionAsText (SectionAnchor xs) = dotSep xs
internalAnchorDescriptionAsText (FigureAnchor (ch, n)) = dotSep [ch, n]
internalAnchorDescriptionAsText (TableAnchor (ch, n)) = dotSep [ch, n]
internalAnchorDescriptionAsText (NoteAnchor (ch, n)) = dotSep [ch, n]
internalAnchorDescriptionAsText (ItemAnchor (ch, ns)) = dotSep (ch:reverse ns)

-- Note: For some anchor types, fallback values are returned --
-- consider throwing an exception in these cases instead.
-- Note: This function is mainly used for 'ItemAnchor' values.
-- | Extract value at the innermost level of an internal anchor.
internalAnchorLocalNum :: InternalAnchor -> Int
internalAnchorLocalNum DocumentAnchor = 0 -- fallback
internalAnchorLocalNum (SectionAnchor xs@(_:_)) = last xs
internalAnchorLocalNum (SectionAnchor []) = 0 -- fallback
internalAnchorLocalNum (FigureAnchor (_, n)) = n
internalAnchorLocalNum (TableAnchor (_, n)) = n
internalAnchorLocalNum (NoteAnchor (_, n)) = n
internalAnchorLocalNum (ItemAnchor (_, n:_)) = n
internalAnchorLocalNum (ItemAnchor (_, [])) = 0 -- fallback

-- Given a list of numbers, format them as an inline string
-- with a separating period between numbers (e.g. \"2.3.1\").
dotSep :: [Int] -> String
dotSep = intercalate "." . map show

-- Given a list of numbers, format them as an inline string
-- with a separating hyphen between numbers (e.g. \"2-3-1\").
hyphenSep :: [Int] -> Text
hyphenSep = T.pack . intercalate "-" . map show

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

-- | A list item with a globally unique number.
--
-- These elements of a 'ListItemBlock' are list items
-- with the special property that they have a unique
-- number based on a global counter. They can be used
-- to model @gb4e@-style linguistic examples.
data ListItem = ListItem InternalAnchor [Block]
  deriving (Eq, Show)

-- | A table row consist of a list of table cells.
type TableRow = [TableCell]

-- Note: Table cells can only contain 'Inline' elements at the moment,
-- but we might have to broaden this to (certain) 'Block' elements.
--
-- | A (logical) table cell can span one or more physical cells.
--
-- Note: 'MultiCell' elements model multi-column cells, not multi-row cells.
data TableCell
  = SingleCell [Inline]
  | MultiCell Int [Inline]
  deriving (Eq, Show)

-- | Block elements in a 'Doc' document.
data Block
  = Para [Inline]
  | Header Level InternalAnchor [Inline]
  | List ListType [[Block]]
  | ListItemBlock [ListItem]
  | QuotationBlock [Block]
  | Figure InternalAnchor Location [Inline]
  | Table InternalAnchor [Inline] [TableRow]
  | SimpleTable [TableRow]
  deriving (Eq, Show)

-- | Inline elements in a 'Doc' document.
data Inline
  = Str String
  | Normal [Inline]
  | Emph [Inline]
  | Space
  | Citation MultiCite (Maybe [Inline])
  | Pointer Label (Maybe Anchor)
  | Note InternalAnchor [Block]
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

-- Warning: does not preserve full content.
-- - strips font styles
-- - drops footnotes
-- - drops prenotes and postnotes in citations
-- | Extract plain character data from an 'Inline' element.
plain :: Inline -> String
plain (Str xs) = xs
plain (Normal is) = concatMap plain is
plain (Emph is) = concatMap plain is
plain Space = " "
plain (Citation (MultiCite _ _ _ xs) _) = T.unpack (plainCites xs)
plain (Pointer label _) = T.unpack (T.concat ["<", label, ">"])
plain Note{} = ""

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
