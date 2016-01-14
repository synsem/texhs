----------------------------------------------------------------------
-- |
-- Module      :  Text.TeX.Context
-- Copyright   :  2015-2016 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  GHC
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
  , runParserWithState
    -- * Parser State
  , getMeta
  , putMeta
  , modifyMeta
    -- ** Combinators
  , choice
  , count
  , sepBy
  , sepBy1
  , list
    -- ** Commands
  , cmd
  , inCmd
  , cmdTwoOblArgs
  , cmdThreeOblArgs
    -- ** Groups
  , grp
  , inGrp
  , grpDown
  , grpUnwrap
  , optNested
    -- ** Low-level parsers
  , eof
  , satisfy
  , dropParents
  ) where

import Text.TeX.Context.Types
import Text.TeX.Context.Walk
