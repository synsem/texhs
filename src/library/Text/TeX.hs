----------------------------------------------------------------------
-- |
-- Module      :  Text.TeX
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  portable
--
-- TeX Parser.
----------------------------------------------------------------------

module Text.TeX
  ( -- * TeX Parser
    readTeX
  , readTeXIO
  ) where

import Text.TeX.Lexer (lexTeX, lexTeXIO)
import Text.TeX.Parser (parseTeX)
import Text.TeX.Parser.Types (TeX)


-- | Parse the named input string to a 'TeX' document.
-- (This is a pure function that ignores IO-related TeX primitives
-- like @\\input@ or @\\year@.)
readTeX :: String -> String -> TeX
readTeX name = parseTeX name . lexTeX name

-- | Parse the named input string to a 'TeX' document,
-- executing embedded IO-related TeX primitives like @\\input@ or @\\year@.
readTeXIO :: String -> String -> IO TeX
readTeXIO name input = parseTeX name <$> lexTeXIO name input
