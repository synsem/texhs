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
  , lexTeXIO
  ) where

import Text.TeX.Lexer.Token (Token)
import Text.TeX.Lexer.TokenParser
  (runLexer, runLexerIO, texLexer, defaultLexerState)

-- | Run TeX lexer on named input string and return a 'Token' list.
-- This is a pure function that ignores IO-related TeX primitives
-- like @\\input@ or @\\year@.
-- (The resulting 'Token' stream is restricted to 'TeXChar' and
-- 'CtrlSeq' elements, i.e. no 'Param' elements.) Exit on failure.
lexTeX :: String -> String -> [Token]
lexTeX name input =
  either (error . show) id $
  runLexer texLexer defaultLexerState name input

-- | Run TeX lexer on named input string and return a 'Token' list,
-- executing embedded IO-related TeX primitives like @\\input@ or @\\year@.
lexTeXIO :: String -> String -> IO [Token]
lexTeXIO name input =
  either (error . show) id <$>
  runLexerIO texLexer defaultLexerState name input
