----------------------------------------------------------------------
-- |
-- Module      :  Text.TeX.Context
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Types and functions for traversing TeX structures
----------------------------------------------------------------------

module Text.TeX.Context
  ( -- * Errors
    TeXDocError(..)
  , ThrowsError
    -- * Contexts
  , TeXContext
  , Context(..)
    -- * Parsers
    -- ** Types
  , Parser
  , runParser
    -- ** Combinators
  , choice
  , count
  , list
    -- ** Commands
  , cmd
  , inCmd
    -- ** Groups
  , grp
  , inGrp
  , grpDown
  , optNested
    -- ** Low-level parsers
  , eof
  , satisfy
  , dropParents
  ) where

import Text.TeX.Context.Types
import Text.TeX.Context.Walk
