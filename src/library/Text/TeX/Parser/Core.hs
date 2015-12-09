----------------------------------------------------------------------
-- |
-- Module      :  Text.TeX.Parser.Core
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  GHC
--
-- Low-level TeX parsers.
----------------------------------------------------------------------

module Text.TeX.Parser.Core
  ( -- * Parser type
    TeXParser
  , runTeXParser
    -- * Fundamental parser
  , satisfy
  ) where

import Text.Parsec (Parsec, ParseError, SourcePos, incSourceColumn, parse, tokenPrim)

import Text.TeX.Lexer.Token (Token)

-------------------- Parser type

-- | Parser for 'Token' input streams,
-- running over Identity monad without user state.
type TeXParser = Parsec [Token] ()

-- | Run a TeX parser on a 'Token' input stream.
runTeXParser :: TeXParser a -> String -> [Token] -> Either ParseError a
runTeXParser p name input = parse p name input

-------------------- Fundamental parsers

-- | Fundamental parser for 'Token' streams.
satisfy :: (Token -> Bool) -> TeXParser Token
satisfy p = tokenPrim show nextpos test
  where
    nextpos pos _ _ = updatePosToken pos
    test = \t -> if p t then Just t else Nothing

-- Increment column for each token, ignore line number.
updatePosToken :: SourcePos -> SourcePos
updatePosToken = flip incSourceColumn 1
