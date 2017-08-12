----------------------------------------------------------------------
-- |
-- Module      :  Text.TeX.Filter.Primitive
-- Copyright   :  2015-2017 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
--
-- Maintainer  :  mschenner.dev@gmail.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- Expansions for primitive TeX commands with syntactic effects
-- (i.e. they expand to textual strings and do not affect the
-- document structure).
----------------------------------------------------------------------

module Text.TeX.Filter.Primitive
  ( syntactic
  , symbols
  ) where

import Data.Map.Strict (Map, fromList)

import Text.TeX.Parser.Types


-- Note: We use (Int,Int) as a simplified ArgSpec,
-- as in "Text.TeX.Parser.Basic".
-- | Primitive TeX commands (syntactic).
syntactic :: Map String ((Int, Int), Args -> TeX)
syntactic = fromList $
  [ ("discretionary", ((0,3), getOblArg 2))  -- discretionary break: ignore
  ]

-- | Primitive TeX symbols (syntactic).
symbols :: Map String String
symbols = fromList $
  [ ("-", "\x00AD")   -- discretionary hyphen: SOFT HYPHEN
  , ("/", "")         -- italic correction: ignore
  ]
