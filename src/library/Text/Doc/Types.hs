----------------------------------------------------------------------
-- |
-- Module      :  Text.Doc.Types
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
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
  , Meta
  , MetaKey
  , MetaValue
  , Content
  , Level
  , Block(..)
  , Inline(..)
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
  , docAuthor
  , docDate
  , plain
    -- * Normalization
  , normalizeInlines
  , stripInlines
  ) where

import Data.List (dropWhileEnd)
--import Data.Map

-------------------- Doc type

-- | A 'Doc' document consist of meta information and content.
data Doc = Doc Meta Content
  deriving (Eq, Show)

-- | Meta information is stored in a key-value map.
-- For now keys and values are simply strings,
-- but values will eventually be more complex
-- (e.g. to allow for a list of authors or emphasis in titles).
type Meta = [(MetaKey, MetaValue)]
--data Meta = Map MetaKey MetaValue

-- | Keys for 'Meta' information.
type MetaKey = String

-- | Values for 'Meta' information.
type MetaValue = String

-- | Content of a 'Doc' document.
type Content = [Block]

-- | Level of a heading.
type Level = Int

-- | Block elements in a 'Doc' document.
data Block
  = Para [Inline]
  | Header Level [Inline]
  | List [[Block]]
  deriving (Eq, Show)

-- | Inline elements in a 'Doc' document.
data Inline
  = Str String
  | Normal [Inline]
  | Emph [Inline]
  | Space
  deriving (Eq, Show)


-------------------- Block predicates

-- | Test whether a 'Block' is a 'Para'.
isPara :: Block -> Bool
isPara (Para _) = True
isPara _ = False

-- | Test whether a 'Block' is a 'Header'.
isHeader :: Block -> Bool
isHeader (Header _ _) = True
isHeader _ = False

-- | Test whether a 'Block' is a 'List'.
isList :: Block -> Bool
isList (List _) = True
isList _ = False


-------------------- Inline predicates

-- | Test whether an 'Inline' is a 'Str'.
isStr :: Inline -> Bool
isStr (Str _) = True
isStr _ = False

-- | Test whether an 'Inline' is a 'Normal'.
isNormal :: Inline -> Bool
isNormal (Normal _) = True
isNormal _ = False

-- | Test whether an 'Inline' is an 'Emph'.
isEmph :: Inline -> Bool
isEmph (Emph _) = True
isEmph _ = False

-- | Test whether an 'Inline' is a 'Space'.
isSpace :: Inline -> Bool
isSpace Space = True
isSpace _ = False


-------------------- Accessor functions

-- | Extract document title.
docTitle :: Doc -> MetaValue
docTitle = lookupMeta "title"

-- | Extract document author.
docAuthor :: Doc -> MetaValue
docAuthor = lookupMeta "author"

-- | Extract document date.
docDate :: Doc -> MetaValue
docDate = lookupMeta "date"

-- Retrieve value for a given meta key.
-- Returns the empty string if lookup failed.
lookupMeta :: MetaKey -> Doc -> MetaValue
lookupMeta key (Doc docmeta _) = maybe "" id (lookup key docmeta)

-- | Extract plain character data from an 'Inline' element.
plain :: Inline -> String
plain (Str xs) = xs
plain (Normal is) = concatMap plain is
plain (Emph is) = concatMap plain is
plain Space = " "


-------------------- Normalization

-- | Conflate adjacent strings.
normalizeInlines :: [Inline] -> [Inline]
normalizeInlines [] = []
normalizeInlines (Str xs : Str ys : zs) = normalizeInlines (Str (xs ++ ys) : zs)
normalizeInlines (xs : ys) = xs : normalizeInlines ys

-- | Strip leading and trailing whitespace.
stripInlines :: [Inline] -> [Inline]
stripInlines = dropWhile isSpace . dropWhileEnd isSpace
