{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
-- Low-level parsers for TeX tokens and re-exports of Parsec parsers.
----------------------------------------------------------------------

module Text.TeX.Lexer.TokenParser.Core
  ( -- * Parser type
    Parser
  , ParserT
  , runParser
    -- * Fundamental parsers
  , charSatCCs
  , satisfyToken
  , satisfyChar
    -- * Stream modifications
  , prependToInput
    -- * Parsec re-exports
    -- ** State
  , getState
  , modifyState
    -- ** Error
  , (<?>)
  , unexpected
    -- ** Combinator
  , (<|>)
  , between
  , choice
  , count
  , eof
  , many
  , many1
  , manyTill
  , option
  , optionMaybe
  , optional
  , try
  ) where

#if MIN_VERSION_base(4,8,0)
-- Prelude already exports everything required from Control.Applicative
#else
import Control.Applicative (Applicative, (<$>))
#endif
import Control.Monad.Trans.Class (MonadTrans)
import Data.Functor.Identity (Identity)
import Text.Parsec (ParsecT, ParseError, tokenPrim)
import qualified Text.Parsec as P
import Text.Parsec.Pos (updatePosChar)

import Text.TeX.Lexer.Catcode (Catcode, hasCatcode)
import Text.TeX.Lexer.Token (Token, getRawChar, isCharSat, hasCC)
import Text.TeX.Lexer.TokenParser.State (LexerState, getCatcodes)


-------------------- Parser type

-- | Parser for TeX input streams, running over Identity monad.
type Parser = ParserT Identity

-- | Parser for TeX input streams.
newtype ParserT m a = ParserT {
    runParserT :: TeXParsecT m a
  } deriving (Functor, Applicative, Monad, MonadTrans)

-- | Run a TeX parser on a 'Char' input stream.
runParser :: Parser a -> LexerState -> String -> String -> Either ParseError a
runParser (ParserT p) st name input = P.runParser p st name (map Left input)

-- ParsecT specialized for TeX input streams.
type TeXParsecT = ParsecT [CharOrToken] LexerState

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


-- Parse a @Right@ stream element.
-- | Parse a verifying token.
satisfyToken :: (Token -> Bool) -> Parser Token
satisfyToken p = satisfy (either (const False) p) >>= \(Right t) -> return t

-- Parse a @Left@ stream element.
-- | Parse a verifying character.
satisfyChar :: (Char -> Bool) -> Parser Char
satisfyChar p = satisfy (either p (const False)) >>= \(Left c) -> return c

-- Fundamental parser for "CharOrToken" streams.
satisfy :: (CharOrToken -> Bool) -> Parser CharOrToken
satisfy p = ParserT $ tokenPrim showToken nextpos test
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


-------------------- Parsec re-exports

---------- lifting helpers

liftP :: (TeXParsecT m a -> TeXParsecT m r) ->
         ParserT m a -> ParserT m r
liftP f (ParserT p) = ParserT (f p)

liftP2 :: (TeXParsecT m a1 -> TeXParsecT m a2 -> TeXParsecT m r) ->
          ParserT m a1 -> ParserT m a2 -> ParserT m r
liftP2 f (ParserT p1) (ParserT p2) = ParserT (f p1 p2)

liftP3 :: (TeXParsecT m a1 -> TeXParsecT m a2 ->
           TeXParsecT m a3 -> TeXParsecT m r) ->
          ParserT m a1 -> ParserT m a2 -> ParserT m a3 -> ParserT m r
liftP3 f (ParserT p1) (ParserT p2) (ParserT p3) = ParserT (f p1 p2 p3)

---------- input

-- | See 'P.getInput' from "Text.Parsec".
getInput :: Parser [CharOrToken]
getInput = ParserT P.getInput

-- | See 'P.setInput' from "Text.Parsec".
setInput :: [CharOrToken] -> Parser ()
setInput = ParserT . P.setInput

---------- state

-- | See 'P.getState' from "Text.Parsec".
getState :: Parser LexerState
getState = ParserT P.getState

-- | See 'P.modifyState' from "Text.Parsec".
modifyState :: (LexerState -> LexerState) -> Parser ()
modifyState = ParserT . P.modifyState

---------- error

infix 0 <?>

-- | See 'P.<?>' from "Text.Parsec".
(<?>) :: Parser a -> String -> Parser a
p <?> msg = liftP (P.<?> msg) p

-- | See 'P.unexpected' from "Text.Parsec".
unexpected :: String -> Parser a
unexpected = ParserT . P.unexpected

---------- combinator

infixr 1 <|>

-- | See 'P.<|>' from "Text.Parsec".
(<|>) :: Parser a -> Parser a -> Parser a
(<|>) = liftP2 (P.<|>)

-- | See 'P.between' from "Text.Parsec".
between :: Parser open -> Parser close -> Parser a -> Parser a
between = liftP3 P.between

-- | See 'P.choice' from "Text.Parsec".
choice :: [Parser a] -> Parser a
choice = ParserT . P.choice . map runParserT

-- | See 'P.count' from "Text.Parsec".
count :: Int -> Parser a -> Parser [a]
count = liftP . P.count

-- | See 'P.eof' from "Text.Parsec".
eof :: Parser ()
eof = ParserT P.eof

-- | See 'P.many' from "Text.Parsec".
many :: Parser a -> Parser [a]
many = liftP P.many

-- | See 'P.many1' from "Text.Parsec".
many1 :: Parser a -> Parser [a]
many1 = liftP P.many1

-- | See 'P.manyTill' from "Text.Parsec".
manyTill :: Parser a -> Parser end -> Parser [a]
manyTill = liftP2 P.manyTill

-- | See 'P.option' from "Text.Parsec".
option :: a -> Parser a -> Parser a
option = liftP . P.option

-- | See 'P.optionMaybe' from "Text.Parsec".
optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe = liftP P.optionMaybe

-- | See 'P.optional' from "Text.Parsec".
optional :: Parser a -> Parser ()
optional = liftP P.optional

-- | See 'P.try' from "Text.Parsec".
try :: Parser a -> Parser a
try = liftP P.try
