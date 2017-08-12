----------------------------------------------------------------------
-- |
-- Module      :  Text.TeX.Context
-- Copyright   :  2015-2017 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
--
-- Maintainer  :  mschenner.dev@gmail.com
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
  , sepEndBy
  , sepEndBy1
  , list
    -- ** Commands
  , cmd
  , inCmd
  , inCmd2
  , inCmd3
  , inCmdOpt2
  , inCmdCheckStar
  , inCmdWithOpts
    -- ** Groups
  , grp
  , inGrp
  , inGrpChoice
  , inMathGrp
  , inSubScript
  , inSupScript
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
