----------------------------------------------------------------------
-- |
-- Module      :  Text.TeX.Lexer.TokenParser
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Parsers for TeX tokens.
----------------------------------------------------------------------

module Text.TeX.Lexer.TokenParser
       ( texLexer
       , defaultLexerStack
       ) where

import Text.TeX.Lexer.TokenParser.Expansion (texLexer)
import Text.TeX.Lexer.TokenParser.State (defaultLexerStack)
