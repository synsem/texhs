----------------------------------------------------------------------
-- |
-- Module      :  Text.Doc.Types
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  portable
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
    -- * Accessor functions
  , docTitle
  , docAuthor
  , docDate
  , plain
  ) where

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
