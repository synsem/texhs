----------------------------------------------------------------------
-- |
-- Module      :  Text.TeX.Lexer.TokenParser
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  GHC
--
-- Parsers for TeX tokens.
----------------------------------------------------------------------

module Text.TeX.Lexer.TokenParser
       ( runLexer
       , runLexerIO
       , texLexer
       , defaultLexerState
       ) where

import Text.TeX.Lexer.TokenParser.Core (runLexer, runLexerIO)
import Text.TeX.Lexer.TokenParser.Execution (texLexer)
import Text.TeX.Lexer.TokenParser.State (defaultLexerState)
