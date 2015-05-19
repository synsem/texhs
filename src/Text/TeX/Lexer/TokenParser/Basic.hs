{-# LANGUAGE CPP #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.TeX.Lexer.TokenParser.Basic
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Basic non-expanding parsers for TeX tokens.
----------------------------------------------------------------------

module Text.TeX.Lexer.TokenParser.Basic
  ( -- * Token parsers
    -- ** General
    singleToken
  , tok
    -- ** 'TeXChar' Parsers
  , char
  , charcc
  , charccno
  , letter
  , string
  , digit
  , octDigit
  , hexDigit
  , bgroup
  , egroup
  , space
  , eol
  , someChar
    -- ** 'CtrlSeq' Parsers
  , ctrlseqNoExpand
  , ctrlseqEq
    -- ** 'Param' Parsers
  , param
    -- * Multi-token parsers
  , tokenNoExpand
  , tokensNoExpand
  , untilTok
  , untilToks
  , balanced
  , balancedCC
    -- * Combinators
  , grouped
  , optGrouped
  , bracketed
    -- * Linebreak parsers
  , eolpar
    -- * Unit parsers
  , skipOptSpace
  , skipOptWhite
  , skipOptCommentsPAR
  , equals
    -- * Number parsers
  , number
  , decToInt
  , octToInt
  , hexToInt
  ) where


#if MIN_VERSION_base(4,8,0)
-- Prelude exports all required operators from Control.Applicative
#else
import Control.Applicative ((<*), (<*>), (*>), (<$), (<$>))
#endif
import Control.Monad (void, when)
import Data.Char (isOctDigit, isDigit, isHexDigit)
import Numeric (readOct, readDec, readHex)
import Text.Parsec
  (getState, modifyState, try, manyTill, between,
   (<|>), many, many1, choice, option, optional, count,
   (<?>), unexpected)

import Text.TeX.Lexer.Catcode
import Text.TeX.Lexer.Token
import Text.TeX.Lexer.TokenParser.Core
import Text.TeX.Lexer.TokenParser.State


-------------------- Token Parsers

-- | Parse a single token.
--
-- Does not accept a group and will not expand macros.
singleToken :: Parser Token
singleToken = skipOptCommentsPAR *>
              (ctrlseqNoExpand <|>
               (eolpar <|> param <|> someChar))

-- | Parse the provided token.
tok :: Token -> Parser Token
tok t = tokT t <|> tokC t

-- 'tok' for tokens.
tokT :: Token -> Parser Token
tokT t = satisfyToken (== t)

-- 'tok' for plain characters.
tokC :: Token -> Parser Token
tokC (TeXChar c i) = charC c i
tokC (CtrlSeq cs active) = ctrlseqEqC cs active
tokC t@(Param i n) = count n (charccC ParamPrefix)
                     *> charC (head (show i)) Other -- assert: 1 <= i <= 9
                     *> return t

-------------------- 'TeXChar' Parsers

-- | Parse the provided character if it has the provided catcode.
char :: Char -> Catcode -> Parser Token
char c i = charT c i <|> charC c i

-- @char@ for tokens.
charT :: Char -> Catcode -> Parser Token
charT c i = satisfyToken (isCharEq c i)

-- @char@ for plain characters.
charC :: Char -> Catcode -> Parser Token
charC c i = do
  cctab <- getCatcodes <$> getState
  void $ satisfyChar (\x -> x==c && hasCatcode i cctab x)
  return (TeXChar c i)

-- | Parse any character with the provided catcode.
--
-- This can be either an already parsed 'TeXChar' with a matching catcode,
-- or a yet unseen plain character (whose catcode matches, according to
-- the current catcode table).
charcc :: Catcode -> Parser Token
charcc i = charccT i <|> charccC i
-- Note: This can also be expressed in terms of @charSatCCs@ as follows:
-- charcc i = charSatCCs (const True) [i] >>= \c -> return (TeXChar c i)

-- | Parse any character that does not have the provided catcode.
charccno :: Catcode -> Parser Token
charccno i = charccTno i <|> charccCno i

-- Parse a 'TeXChar' with the provided catcode.
charccT :: Catcode -> Parser Token
charccT i = satisfyToken (hasCC i)

-- Parse a 'TeXChar' that does not have the provided catcode.
charccTno :: Catcode -> Parser Token
charccTno i = satisfyToken (not . hasCC i)

-- Parse a plain character if it fits the provided catcode,
-- and return it as a 'TeXChar'.
charccC :: Catcode -> Parser Token
charccC i = do
  cctab <- getCatcodes <$> getState
  c <- satisfyChar (hasCatcode i cctab)
  return (TeXChar c i)

-- Parse a plain character that does not have the provided catcode,
-- and return it as a 'TeXChar'.
charccCno :: Catcode -> Parser Token
charccCno i = do
  cctab <- getCatcodes <$> getState
  c <- satisfyChar (not . hasCatcode i cctab)
  return (TeXChar c (catcodeOf c cctab))

-- | Parse the provided character if it has catcode 'Letter'.
letter :: Char -> Parser Token
letter = flip char Letter

-- | Parse the provided string.
--
-- This will only accept characters with catcode 'Letter'.
string :: String -> Parser String
string = foldr op (return "")
  where op = \c -> (<*>) ((:) <$> plainLetter c)
        plainLetter = \c -> (c <$ letter c)

-- | Parse a decimal digit.
digit :: Parser Char
digit = charSatCCs isDigit [Other]

-- | Parse an octal digit.
octDigit :: Parser Char
octDigit = charSatCCs isOctDigit [Other]

-- | Parse a hexadecimal digit.
--
-- Note: TeX wants cc(0-9) = Other, cc(a-f)= Other|Letter.
-- We are more liberal and only require cc(0-9a-f) = Other|Letter.
hexDigit :: Parser Char
hexDigit = charSatCCs isHexDigit [Other, Letter]

-- | Parse an explicit bgroup token.
bgroup :: Parser Token
bgroup = charcc Bgroup <* modifyState (pushGroup AnonymousGroup)

-- | Parse an explicit egroup token.
egroup :: Parser Token
egroup = charcc Egroup <* modifyState (popGroup AnonymousGroup)

-- | Parse an explicit space token.
space :: Parser Token
space = charcc Space

-- | Parse an explicit end-of-line token.
eol :: Parser Token
eol = charcc Eol


-- | Parse any character if its catcode is in the provided list.
anyCharCC :: [Catcode] -> Parser Token
anyCharCC ccs = choice $ map charcc ccs

-- | Parse a 'TeXChar' of any /passive/ catcode (see 'catcodesPassive').
someChar :: Parser Token
someChar = anyCharCC catcodesPassive

-- | Parse a 'TeXChar' of any /allowed/ catcode (see 'catcodesAllowed').
anyChar :: Parser Token
anyChar = anyCharCC catcodesAllowed

-- | Parse a 'TeXChar' that is optionally prefixed by an 'Escape' char.
anyCharOptEsc :: Parser Token
anyCharOptEsc = anyCharCC catcodesNonescaped <|> anyEscapedChar

-- | Parse a 'TeXChar' that is prefixed by an 'Escape' char.
-- In terms of tokens, this is a single character 'CtrlSeq'.
--
-- If we encounter a multi-character control sequence, TeX fails
-- with the error message /Improper alphabetic constant/
-- (e.g. for the input: @\\number`\\ab@).
anyEscapedChar :: Parser Token
anyEscapedChar = do
  (CtrlSeq cs _) <- ctrlseqNoExpand
  if length cs == 1
    then return (TeXChar (head cs) Other) -- Note: coercing to catcode 'Other' here
    else unexpected $ cs ++ " (where a single character was required). "
         ++ "TeX: Improper alphabetic constant."

-------------------- 'CtrlSeq' Parsers

-- | Parse a control sequence without trying to expand it.
ctrlseqNoExpand :: Parser Token
ctrlseqNoExpand = ctrlseqT <|> ctrlseqC <|> activeC

ctrlseqT :: Parser Token
ctrlseqT = satisfyToken isCtrlSeq

ctrlseqC :: Parser Token
ctrlseqC = do
  -- Note: we are using the internal plain char parsers here
  -- because parsed tokens cannot compose to a 'CtrlSeq'.
  void $ charccC Escape
  cs <- (many1 (charccC Letter) <* skipSpacePAR)
        <|> count 1 anyChar
        <?> "control sequence"
  return (CtrlSeq (map getRawChar cs) False)

activeC :: Parser Token
activeC = charccC Active >>= \(TeXChar c _) -> return (CtrlSeq [c] True)

-- | Parse a control sequence with the provided name.
ctrlseqEq :: String -> Bool -> Parser Token
ctrlseqEq cs active = ctrlseqEqT cs active <|> ctrlseqEqC cs active

ctrlseqEqT :: String -> Bool -> Parser Token
ctrlseqEqT cs active = tok (CtrlSeq cs active)

ctrlseqEqC :: String -> Bool -> Parser Token
ctrlseqEqC name True = let c = head name -- for active chars @length name == 1@
                       in char c Active *> return (CtrlSeq [c] True)
ctrlseqEqC name False = do
  void $ charccC Escape
  cs <- string name <* skipSpacePAR
  return (CtrlSeq cs False)

-------------------- 'Param' Parsers

-- | Parse a TeX parameter token.
--
-- These are only allowed in macro definitions.
-- Typical examples: @\#1@, @\#2@, @\#\#1@.
param :: Parser Token
param = paramT <|> paramC

paramT :: Parser Token
paramT = satisfyToken isParam

paramC :: Parser Token
paramC = do
  n <- length <$> many1 (charccC ParamPrefix)
  i <- read <$> count 1 (satisfyChar isDigit)
  return (Param i n)

-------------------- Multi-token parsers

-- | Parse a /logical unit/ of tokens without expanding them.
-- This is either a single 'Token' or a group of tokens.
tokenNoExpand :: Parser [Token]
tokenNoExpand =
  skipOptCommentsPAR *>
  (groupNoExpand <|> count 1
   (ctrlseqNoExpand <|> eolpar <|>
    param <|> someChar))

-- | Parse many logical units of tokens without expanding them.
-- (See 'tokenNoExpand'.)
tokensNoExpand :: Parser [Token]
tokensNoExpand = concat <$> many tokenNoExpand

-- Parse a balanced TeX group as a flat token list including delimiters.
groupNoExpand :: Parser [Token]
groupNoExpand = (fmap (++) . (:)) <$> bgroup <*> tokensNoExpand <*> count 1 egroup

-- | Parse tokens (without expansion) until you hit the specified delimiter.
-- The delimiter is not included in the result.
untilTok :: Token -> Parser [Token]
untilTok t = concat <$> manyTill tokenNoExpand (tok t)

-- | Parse tokens (without expansion) until you hit the specified token sequence.
-- The delimiting sequence is not included in the result.
untilToks :: [Token] -> Parser [Token]
untilToks ts = concat <$> manyTill tokenNoExpand (try (mapM_ tok ts))

-- | Parse a balanced group of tokens (without expansion)
-- between the provided opening and closing tokens.
-- The (outermost) delimiters are not included in the result.
balanced :: Token -> Token -> Parser [Token]
balanced open close =
  tok open *> balancedEnd open close

-- Helper function for 'balanced'. Skips delimiters.
balancedEnd :: Token -> Token -> Parser [Token]
balancedEnd open close =
  ([] <$ tok close) <|>
  ((++) <$> (balancedInner open close <|> tokenNoExpand)
   <*> balancedEnd open close)

-- Helper function for 'balanced'. Keeps delimiters.
balancedInner :: Token -> Token -> Parser [Token]
balancedInner open close =
  (:) <$> tok open <*> balancedInnerEnd open close

-- Helper function for 'balanced'. Keeps delimiters.
balancedInnerEnd :: Token -> Token -> Parser [Token]
balancedInnerEnd open close =
  count 1 (tok close) <|>
  ((++) <$> (balancedInner open close <|> tokenNoExpand)
   <*> balancedInnerEnd open close)

-- | Parse a balanced group of tokens (without expansion)
-- between tokens with the provided opening and closing catcodes.
-- The (outermost) delimiters are not included in the result.
balancedCC :: Catcode -> Catcode -> Parser [Token]
balancedCC open close =
  charcc open *> balancedCCEnd open close

-- Helper function for 'balancedCC'. Skips delimiters.
balancedCCEnd :: Catcode -> Catcode -> Parser [Token]
balancedCCEnd open close =
  ([] <$ charcc close) <|>
  ((++) <$> (balancedCCInner open close <|> tokenNoExpand)
   <*> balancedCCEnd open close)

-- Helper function for 'balancedCC'. Keeps delimiters.
balancedCCInner :: Catcode -> Catcode -> Parser [Token]
balancedCCInner open close =
  (:) <$> charcc open <*> balancedCCInnerEnd open close

-- Helper function for 'balancedCC'. Keeps delimiters.
balancedCCInnerEnd :: Catcode -> Catcode -> Parser [Token]
balancedCCInnerEnd open close =
  count 1 (charcc close) <|>
  ((++) <$> (balancedCCInner open close <|> tokenNoExpand)
   <*> balancedCCInnerEnd open close)

-------------------- Combinators

-- | Apply a parser in a TeX group (between 'Bgroup' and 'Egroup').
-- The delimiters are not included in the result.
grouped :: Parser a -> Parser a
grouped = between bgroup egroup

-- | Apply a parser in a TeX group (see 'grouped') or directly.
optGrouped :: Parser a -> Parser a
optGrouped p = grouped p <|> p

-- | Apply a parser between a pair of square brackets.
-- The delimiters are not included in the result.
bracketed :: Parser a -> Parser a
bracketed = between (char '[' Other) (char ']' Other)

-------------------- Linebreak parsers

-- | Parse an 'Eol' char or, if followed by an empty line, a par token.
-- This also skips leading space on the following line.
eolpar :: Parser Token
eolpar = parT <|> ((eol <* skipOptSpace) >>= flip option par)

-- Parse a paragraph break (via a 'parTok' token or via an empty line
-- after a previously seen 'Eol' character).
par :: Parser Token
par = parT <|> (parTok <$ (eol <* skipOptWhite))

-- Parse a par token (and skip all subsequent whitespace).
parT :: Parser Token
parT = tokT parTok <* skipOptWhite

-------------------- Unit parsers

-- | Skip optional 'Space' chars.
skipOptSpace :: Parser ()
skipOptSpace = void $ many space

-- | Skip all whitespace ('Space' and 'Eol' chars) and comments.
skipOptWhite :: Parser ()
skipOptWhite = void $ many (void (space <|> eol) <|> skipComments)

-- Skip a single comment, including the trailing 'Eol' and any leading
-- space on the following line.
skipSingleComment :: Parser ()
skipSingleComment = charcc Comment *> many (charccno Eol) *> eol *> skipOptSpace

-- Skip at least one comment.
skipComments :: Parser ()
skipComments = void $ many1 skipSingleComment

-- Skip at least one comment.
--
-- NOTE: If followed by an empty line, push a par token back into the stream.
skipCommentsPAR :: Parser ()
skipCommentsPAR = do
  t <- many1 skipSingleComment *> option (TeXChar '\n' Eol) par
  when (isCtrlSeq t) -- true iff par token
    (prependToInput [t])

-- | Skip optional comments.
--
-- NOTE: If followed by an empty line, push a par token back into the stream.
skipOptCommentsPAR :: Parser ()
skipOptCommentsPAR = option () skipCommentsPAR

-- Skip an optional 'Eol' character and surrounding space.
--
-- NOTE: If followed by an empty line, push a par token back into the stream.
skipSpacePAR :: Parser ()
skipSpacePAR = do
  t <- skipOptSpace *> option (TeXChar '\n' Eol) eolpar
  when (isCtrlSeq t) -- true iff par token
    (prependToInput [t])

-- | Parse an optional equals sign.
equals :: Parser ()
equals = skipOptSpace *> optional (char '=' Other) *> skipOptSpace

-------------------- Number parsers

-- | Parse a number.
number :: Parser Int
number = choice [readcc, chrnum, hexnum, octnum, decnum]

-- Parse an alpha constant.
chrnum :: Parser Int
chrnum = (fromEnum . getRawChar) <$> (tok ordPrefix *> anyCharOptEsc)

-- Parse a hex constant.
hexnum :: Parser Int
hexnum = hexToInt <$> (tok hexPrefix *> many1 hexDigit)

-- Parse an octal constant.
octnum :: Parser Int
octnum = octToInt <$> (tok octPrefix *> many1 octDigit)

-- Parse a decimal constant.
decnum :: Parser Int
decnum = decToInt <$> many1 digit

-- | Convert a string of digits to an integer.
decToInt :: String -> Int
decToInt cs = case readDec cs of
  [(i,[])] -> i
  _ -> error $ "Cannot read dec number: " ++ cs

-- | Convert a string of hex digits to an integer.
hexToInt :: String -> Int
hexToInt cs = case readHex cs of
  [(i,[])] -> i
  _ -> error $ "Cannot read hex number: " ++ cs

-- | Convert a string of octal digits to an integer.
octToInt :: String -> Int
octToInt cs = case readOct cs of
  [(i,[])] -> i
  _ -> error $ "Cannot read oct number: " ++ cs

-- Retrieve the current catcode of a character as an integer.
readcc :: Parser Int
readcc = do
  cctab <- getCatcodes <$> getState
  ch <- toEnum <$> (ctrlseqEq "catcode" False *> number)
  return $ fromEnum (catcodeOf ch cctab)
