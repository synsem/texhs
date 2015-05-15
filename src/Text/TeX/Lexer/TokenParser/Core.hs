----------------------------------------------------------------------
-- |
-- Module      :  Text.TeX.Lexer.TokenParser.Core
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Low-level parsers for TeX tokens.
----------------------------------------------------------------------

module Text.TeX.Lexer.TokenParser.Core
  ( -- * Parser type
    Parser
    -- * Fundamental parsers
  , charSatCCs
  , satisfyToken
  , satisfyChar
    -- * Stream modifications
  , prependToInput
  ) where

import Control.Applicative hiding ((<|>))
import Text.Parsec (Parsec, tokenPrim, getInput, setInput, getState, (<|>))
import Text.Parsec.Pos (updatePosChar)

import Text.TeX.Lexer.Catcode (Catcode, hasCatcode)
import Text.TeX.Lexer.Token (Token, getRawChar, isCharSat, hasCC)
import Text.TeX.Lexer.TokenParser.State (LexerStack, getCatcodes)


-------------------- Parser type

-- | Parser for TeX input streams.
type Parser = Parsec [CharOrToken] LexerStack

-- The input to the lexer/parser is a stream of @CharOrToken@
-- elements. These are either (1) unparsed and yet unseen raw @Char@
-- elements (i.e. characters that need to be assigned a catcode or to
-- be assembled into control sequences) or (2) already parsed @Token@
-- elements (e.g. characters with existing catcode assignment, like
-- they are stored in macros). All @Token@ elements in this stream are
-- the result of macro expansion, and they always form a prefix of the
-- stream (i.e. they never appear after a raw @Char@ element).

-- NOTE: We are using @Either@ to hold two equally relevant types.
-- In particular, @Left@ values are not to be interpreted as errors.
-- | This is the type of elements of a TeX input stream.
type CharOrToken = Either Char Token


-------------------- Fundamental parsers

-- | Parse a character that both satisfies the provided property
-- and has one of the listed catcodes.
charSatCCs :: (Char -> Bool) -> [Catcode] -> Parser Char
charSatCCs p ccs = charSatCCsT p ccs <|> charSatCCsC p ccs

charSatCCsT :: (Char -> Bool) -> [Catcode] -> Parser Char
charSatCCsT p ccs = getRawChar <$> satisfyToken
                    (\t -> isCharSat p t && any (`hasCC` t) ccs)

charSatCCsC :: (Char -> Bool) -> [Catcode] -> Parser Char
charSatCCsC p ccs = do
  cctab <- getCatcodes <$> getState
  satisfyChar (\x -> p x && any (\cc -> hasCatcode cc cctab x) ccs)



-- | Parse a @Right@ stream element.
satisfyToken :: (Token -> Bool) -> Parser Token
satisfyToken p = satisfy (either (const False) p) >>= \(Right t) -> return t

-- | Parse a @Left@ stream element.
satisfyChar :: (Char -> Bool) -> Parser Char
satisfyChar p = satisfy (either p (const False)) >>= \(Left c) -> return c

-- Fundamental parser for "CharOrToken" streams.
satisfy :: (CharOrToken -> Bool) -> Parser CharOrToken
satisfy p = tokenPrim showToken nextpos test
  where
    showToken = show
    nextpos = \pos t _ -> case t of
      Left c -> updatePosChar pos c
      Right _ -> pos -- don't increment position
    test = \t -> if p t then Just t else Nothing


-------------------- Stream modifications

-- | Prepend a list of tokens to the input stream.
prependToInput :: [Token] -> Parser ()
prependToInput xs = (map Right xs ++) <$> getInput >>= setInput
