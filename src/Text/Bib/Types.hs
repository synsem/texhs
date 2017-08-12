{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.Bib.Types
-- Copyright   :  2015-2017 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
--
-- Maintainer  :  mschenner.dev@gmail.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- Types for representing bibliographic information.
----------------------------------------------------------------------

module Text.Bib.Types
  ( -- * Types
    BibDB
  , BibFieldMap
  , BibFieldName
  , BibFieldValue(..)
  , BibEntry(..)
  , Agent(..)
  , CiteKey
    -- * Accessor functions
  , getBibField
  , getBibAgents
  , getBibList
  , getBibLiteral
  , getBibRaw
    -- * Manipulation functions
  , deleteField
  , deleteFields
  ) where

import Control.Applicative
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Text (Text)

import Text.Doc.Types (Inline(Space), CiteKey)


-------------------- Types

-- | A bibliographic database maps citekeys to entries.
type BibDB = Map CiteKey BibEntry

-- | A map of fields in a 'BibEntry'.
type BibFieldMap = Map BibFieldName BibFieldValue

-- | A name of a BibField.
type BibFieldName = Text

-- | A value of a BibField.
data BibFieldValue
  = AgentList [Agent]        -- fields containing a name list
  | LiteralList [[Inline]]   -- fields containing a literal list
  | LiteralField [Inline]    -- unstructured fields (aka literal fields)
  | RawField Text            -- internal fields that are not parsed
  deriving (Eq, Show)

-- | An entry in a bibliographic database.
data BibEntry = BibEntry
  { bibType   :: Text         -- e.g. "article", "book"
  , bibFields :: BibFieldMap
  } deriving (Eq, Show)

-- Compare by name (agents), year and title.
--
-- Note: Years are sorted by their string representation,
-- not their numeric value.
instance Ord BibEntry where
  compare =
    comparing agent <>
    comparing year <>
    comparing title
    where
      agent e = (intercalate [Space] . map agentLastFirst) <$>
        ( getBibAgents "author" e <|>
          getBibAgents "editor" e )
      year = getBibLiteral "year"
      title = getBibLiteral "title"
      agentLastFirst (Agent f p l s) = intercalate [Space] [l,f,p,s]

-- | Representation of person names (e.g. author or editor).
data Agent = Agent
  { agentFirst  :: [Inline]   -- first names
  , agentPrefix :: [Inline]   -- prefix names (von)
  , agentLast   :: [Inline]   -- last names
  , agentSuffix :: [Inline]   -- suffix names (jr)
  } deriving (Eq, Show)


-------------------- Accessor functions

-- | Retrieve a field value from a 'BibEntry'.
getBibField :: BibFieldName -> BibEntry -> Maybe BibFieldValue
getBibField key entry = M.lookup key (bibFields entry)

-- | Retrieve an unwrapped name list from a 'BibEntry'.
getBibAgents :: BibFieldName -> BibEntry -> Maybe [Agent]
getBibAgents key entry = getBibField key entry >>= extractBibAgents

-- | Retrieve an unwrapped literal list from a 'BibEntry'.
getBibList :: BibFieldName -> BibEntry -> Maybe [[Inline]]
getBibList key entry = getBibField key entry >>= extractBibList

-- | Retrieve an unwrapped literal field from a 'BibEntry'.
getBibLiteral :: BibFieldName -> BibEntry -> Maybe [Inline]
getBibLiteral key entry = getBibField key entry >>= extractBibLiteral

-- | Retrieve an unwrapped raw field from a 'BibEntry'.
getBibRaw :: BibFieldName -> BibEntry -> Maybe Text
getBibRaw key entry = getBibField key entry >>= extractBibRaw

-- Extract name list from a field value.
extractBibAgents :: BibFieldValue -> Maybe [Agent]
extractBibAgents (AgentList xs) = Just xs
extractBibAgents _ = Nothing

-- Extract literal list from a field value.
extractBibList :: BibFieldValue -> Maybe [[Inline]]
extractBibList (LiteralList xs) = Just xs
extractBibList _ = Nothing

-- Extract literal field from a field value.
extractBibLiteral :: BibFieldValue -> Maybe [Inline]
extractBibLiteral (LiteralField xs) = Just xs
extractBibLiteral _ = Nothing

-- Extract raw field from a field value.
extractBibRaw :: BibFieldValue -> Maybe Text
extractBibRaw (RawField xs) = Just xs
extractBibRaw _ = Nothing


---------- Manipulation functions

-- | Delete a field from a bib entry.
deleteField :: BibFieldName -> BibEntry -> BibEntry
deleteField name (BibEntry btype bfields) =
  BibEntry btype (M.delete name bfields)

-- | Delete a list of fields from a bib entry.
deleteFields :: [BibFieldName] -> BibEntry -> BibEntry
deleteFields names (BibEntry btype bfields) =
  BibEntry btype (foldr M.delete bfields names)
