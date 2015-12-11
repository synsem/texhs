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
  , Meta(..)
  , defaultMeta
  , HasMeta(..)
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
  , docAuthors
  , docDate
  , plain
    -- * Normalization
  , normalizeInlines
  , stripInlines
  ) where

import Data.List (dropWhileEnd)


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
  } deriving (Eq, Show)

-- | Default (empty) meta information of a document.
defaultMeta :: Meta
defaultMeta = Meta
  { metaTitle = []
  , metaAuthors = []
  , metaDate = []
  }

-- | A class for document types that hold meta information.
class HasMeta d where
  -- | Extract meta information.
  docMeta :: d -> Meta

instance HasMeta Doc where
  docMeta (Doc meta _) = meta


-------------------- Content type

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
docTitle :: HasMeta d => d -> [Inline]
docTitle = metaTitle . docMeta

-- | Extract document authors.
docAuthors :: HasMeta d => d -> [[Inline]]
docAuthors = metaAuthors . docMeta

-- | Extract document date.
docDate :: HasMeta d => d -> [Inline]
docDate = metaDate . docMeta

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
