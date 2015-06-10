----------------------------------------------------------------------
-- |
-- Module      :  Text.TeX.Lexer
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Lexer for TeX and LaTeX documents.
----------------------------------------------------------------------

module Text.TeX.Lexer
  ( -- * TeX Lexer
    lexTeX
  ) where

import Text.TeX.Lexer.Token (Token)
import Text.TeX.Lexer.TokenParser (runParser, texLexer, defaultLexerState)

-- | Run TeX lexer on named input string and return a 'Token' list.
-- (The resulting 'Token' stream is restricted to 'TeXChar' and
-- 'CtrlSeq' elements, i.e. no 'Param' elements.) Exit on failure.
lexTeX :: String -> String -> [Token]
lexTeX name input = case runParser texLexer defaultLexerState name input of
  Left l  -> error (show l)
  Right r -> r
