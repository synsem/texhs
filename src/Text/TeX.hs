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
  ) where

import Text.TeX.Lexer (lexTeX)
import Text.TeX.Parser (parseTeX)
import Text.TeX.Parser.Types (TeX)


-- | Parse the named input string to a 'TeX' document.
readTeX :: String -> String -> TeX
readTeX name = parseTeX name . lexTeX name
