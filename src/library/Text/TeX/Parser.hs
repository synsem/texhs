----------------------------------------------------------------------
-- |
-- Module      :  Text.TeX.Parser
-- Copyright   :  2015-2016 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  GHC
--
-- Parser for TeX token streams.
----------------------------------------------------------------------

module Text.TeX.Parser
  ( -- * TeX Parser
    parseTeX
  ) where

import Text.TeX.Filter (normalize, resolveSymbols)
import Text.TeX.Lexer.Token (Token)
import Text.TeX.Parser.Types (TeX)
import Text.TeX.Parser.Core (runTeXParser)
import Text.TeX.Parser.Basic (texParser)


-- | Run 'TeX' parser on named input 'Token' stream.
parseTeX :: String -> [Token] -> TeX
parseTeX name input = case runTeXParser texParser name input of
  Left l  -> error (show l)
  Right r -> (normalize . resolveSymbols) r
