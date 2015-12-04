----------------------------------------------------------------------
-- |
-- Module      :  Text.Bib.Types
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Types for representing bibliographic information.
----------------------------------------------------------------------

module Text.Bib.Types
  ( -- * Types
    BibDB
  , BibEntry(..)
  , Agent(..)
  , CiteKey
    -- * Accessor functions
  , getBibAgents
  , getBibList
  , getBibField
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)

import Text.Doc.Types (Inline)


-------------------- Types

-- | A bibliographic database maps citekeys to entries.
type BibDB = Map CiteKey BibEntry

-- | A key for entry lookups in a 'BibDB'.
type CiteKey = Text

-- | An entry in a bibliographic database.
data BibEntry = BibEntry
  { bibType   :: Text                  -- e.g. "article", "book"
  , bibAgents :: [(Text, [Agent])]     -- fields containing a name list
  , bibLists  :: [(Text, [[Inline]])]  -- fields containing a literal list
  , bibFields :: [(Text, [Inline])]    -- other fields (literal fields)
  } deriving (Eq, Show)

-- | Representation of person names (e.g. author or editor).
data Agent = Agent
  { agentFirst  :: [Inline]   -- first names
  , agentPrefix :: [Inline]   -- prefix names (von)
  , agentLast   :: [Inline]   -- last names
  , agentSuffix :: [Inline]   -- suffix names (jr)
  } deriving (Eq, Show)


-------------------- Accessor functions

-- | Retrieve an 'Agent' (name list) field from a 'BibEntry'.
getBibAgents :: Text -> BibEntry -> Maybe [Agent]
getBibAgents key entry = lookup key (bibAgents entry)

-- | Retrieve a field containing a literal list from a 'BibEntry'.
getBibList :: Text -> BibEntry -> Maybe [[Inline]]
getBibList key entry = lookup key (bibLists entry)

-- | Retrieve a literal field from a 'BibEntry'.
getBibField :: Text -> BibEntry -> Maybe [Inline]
getBibField key entry = lookup key (bibFields entry)
