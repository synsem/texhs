{-# LANGUAGE CPP #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.TeX.Lexer.TokenParser.Basic
-- Copyright   :  2015-2016 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  GHC
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
  , bgroup
  , egroup
  , someChar
    -- ** 'CtrlSeq' Parsers
  , ctrlseqNoExpand
  , ctrlseqEq
    -- ** 'Param' Parsers
  , param
    -- * Multi-token parsers
  , tokenNoExpand
  , nextTokenNoExpand
  , tokensNoExpand
  , untilTok
  , untilToks
  , balanced
  , balancedCC
    -- * Combinators
  , grouped
  , optGrouped
  , bracketed
  , parseUntilNonEmpty
    -- * Whitespace parsers
  , anyWhite
  , skipOptSpace
  , skipSpaceExceptPar
    -- * Unit parsers
  , equals
    -- * Char parsers
  , rawChar
  , string
  , filename
  , digit
  , octDigit
  , hexDigit
    -- * Number parsers
  , number
  , singleDigit
  ) where


#if MIN_VERSION_base(4,8,0)
-- Prelude exports all required operators from Control.Applicative
#else
import Control.Applicative ((<*), (<*>), (*>), (<$), (<$>))
#endif
import Control.Monad (void, when)
import Data.Char (isOctDigit, isDigit, isHexDigit, isSpace, isControl)
import Numeric (readOct, readDec, readHex)

import Text.TeX.Lexer.Catcode
import Text.TeX.Lexer.Token
import Text.TeX.Lexer.TokenParser.Core
import Text.TeX.Lexer.TokenParser.State


-------------------- Token Parsers

-- | Parse a single token.
--
-- Does not accept a group and will not expand macros.
singleToken :: Monad m => LexerT m Token
singleToken = option [] anyWhite >>= \xs ->
  if null xs
  then choice [ctrlseqNoExpand, param, someChar]
  else return (head xs)

-- | Parse the provided token.
tok :: Monad m => Token -> LexerT m Token
tok t = tokT t <|> tokC t

-- 'tok' for tokens.
tokT :: Monad m => Token -> LexerT m Token
tokT t = satisfyToken (== t)

-- 'tok' for plain characters.
tokC :: Monad m => Token -> LexerT m Token
tokC (TeXChar c i) = charC c i
tokC (CtrlSeq cs active) = ctrlseqEqC cs active
tokC t@(Param i n) = count n (charccC ParamPrefix)
                     *> charC (head (show i)) Other -- assert: 1 <= i <= 9
                     *> return t

-------------------- 'TeXChar' Parsers

-- | Parse the provided character if it has the provided catcode.
char :: Monad m => Char -> Catcode -> LexerT m Token
char c i = charT c i <|> charC c i

-- @char@ for tokens.
charT :: Monad m => Char -> Catcode -> LexerT m Token
charT c i = satisfyToken (isCharEq c i)

-- @char@ for plain characters.
charC :: Monad m => Char -> Catcode -> LexerT m Token
charC c i = do
  cctab <- getCatcodes <$> getState
  void $ satisfyChar (\x -> x==c && hasCatcode i cctab x)
  return (TeXChar c i)

-- | Parse any character with the provided catcode.
--
-- This can be either an already parsed 'TeXChar' with a matching catcode,
-- or a yet unseen plain character (whose catcode matches, according to
-- the current catcode table).
charcc :: Monad m => Catcode -> LexerT m Token
charcc i = charccT i <|> charccC i
-- Note: This can also be expressed in terms of @satisfyCharCC@ as follows:
-- charcc i = satisfyCharCC (const True) [i] >>= \c -> return (TeXChar c i)

-- | Parse any character that does not have the provided catcode.
charccno :: Monad m => Catcode -> LexerT m Token
charccno i = charccTno i <|> charccCno i

-- Parse a 'TeXChar' with the provided catcode.
charccT :: Monad m => Catcode -> LexerT m Token
charccT i = satisfyToken (hasCC i)

-- Parse a 'TeXChar' that does not have the provided catcode.
charccTno :: Monad m => Catcode -> LexerT m Token
charccTno i = satisfyToken (not . hasCC i)

-- Parse a plain character if it fits the provided catcode,
-- and return it as a 'TeXChar'.
charccC :: Monad m => Catcode -> LexerT m Token
charccC i = do
  cctab <- getCatcodes <$> getState
  c <- satisfyChar (hasCatcode i cctab)
  return (TeXChar c i)

-- Parse a plain character that does not have the provided catcode,
-- and return it as a 'TeXChar'.
charccCno :: Monad m => Catcode -> LexerT m Token
charccCno i = do
  cctab <- getCatcodes <$> getState
  c <- satisfyChar (not . hasCatcode i cctab)
  return (TeXChar c (catcodeOf c cctab))

-- | Parse the provided character if it has catcode 'Letter'.
letter :: Monad m => Char -> LexerT m Token
letter = flip char Letter

-- | Parse an explicit bgroup token.
bgroup :: Monad m => LexerT m Token
bgroup = charcc Bgroup <* modifyState (pushGroup AnonymousGroup)

-- | Parse an explicit egroup token.
egroup :: Monad m => LexerT m Token
egroup = charcc Egroup <* modifyState (popGroup AnonymousGroup)


-- | Parse any character if its catcode is in the provided list.
anyCharCC :: Monad m => [Catcode] -> LexerT m Token
anyCharCC ccs = choice $ map charcc ccs

-- | Parse a 'TeXChar' of any /passive/ catcode (see 'catcodesPassive').
someChar :: Monad m => LexerT m Token
someChar = anyCharCC catcodesPassive

-- | Parse a 'TeXChar' of any /allowed/ catcode (see 'catcodesAllowed').
anyChar :: Monad m => LexerT m Token
anyChar = anyCharCC catcodesAllowed

-- | Parse a 'TeXChar' that is optionally prefixed by an 'Escape' char.
anyCharOptEsc :: Monad m => LexerT m Token
anyCharOptEsc = anyCharCC catcodesNonescaped <|> anyEscapedChar

-- | Parse a 'TeXChar' that is prefixed by an 'Escape' char.
-- In terms of tokens, this is a single character 'CtrlSeq'.
--
-- If we encounter a multi-character control sequence, TeX fails
-- with the error message /Improper alphabetic constant/
-- (e.g. for the input: @\\number`\\ab@).
anyEscapedChar :: Monad m => LexerT m Token
anyEscapedChar = do
  (CtrlSeq cs _) <- ctrlseqNoExpand
  if length cs == 1
    then return (TeXChar (head cs) Other) -- Note: coercing to catcode 'Other' here
    else unexpected $ cs ++ " (where a single character was required). "
         ++ "TeX: Improper alphabetic constant."

-------------------- 'CtrlSeq' Parsers

-- | Parse a control sequence without trying to expand it.
-- Always returns a 'CtrlSeq' token.
ctrlseqNoExpand :: Monad m => LexerT m Token
ctrlseqNoExpand = ctrlseqT <|> ctrlseqC <|> activeC

ctrlseqT :: Monad m => LexerT m Token
ctrlseqT = satisfyToken isCtrlSeq

ctrlseqC :: Monad m => LexerT m Token
ctrlseqC = do
  -- Note: we are using the internal plain char parsers here
  -- because parsed tokens cannot compose to a 'CtrlSeq'.
  void $ charccC Escape
  cs <- (many1 (charccC Letter) <* skipSpaceExceptPar)
        <|> count 1 anyChar
        <?> "control sequence"
  return (CtrlSeq (map getRawChar cs) False)

activeC :: Monad m => LexerT m Token
activeC = charccC Active >>= \(TeXChar c _) -> return (CtrlSeq [c] True)

-- | Parse a control sequence with the provided name.
ctrlseqEq :: Monad m => String -> Bool -> LexerT m Token
ctrlseqEq cs active = ctrlseqEqT cs active <|> ctrlseqEqC cs active

ctrlseqEqT :: Monad m => String -> Bool -> LexerT m Token
ctrlseqEqT cs active = tok (CtrlSeq cs active)

ctrlseqEqC :: Monad m => String -> Bool -> LexerT m Token
ctrlseqEqC name True = let c = head name -- for active chars @length name == 1@
                       in char c Active *> return (CtrlSeq [c] True)
ctrlseqEqC name False = do
  void $ charccC Escape
  cs <- string name <* skipSpaceExceptPar
  return (CtrlSeq cs False)

-------------------- 'Param' Parsers

-- | Parse a TeX parameter token.
--
-- These are only allowed in macro definitions.
-- Typical examples: @\#1@, @\#2@, @\#\#1@.
param :: Monad m => LexerT m Token
param = paramT <|> paramC

paramT :: Monad m => LexerT m Token
paramT = satisfyToken isParam

paramC :: Monad m => LexerT m Token
paramC = do
  n <- length <$> many1 (charccC ParamPrefix)
  i <- read <$> count 1 (satisfyChar isDigit)
  return (Param i n)

-------------------- Multi-token parsers

-- | Parse a /logical unit/ of tokens without expanding them.
--
-- Possible return values:
--
-- * empty list: resulting from an intra-paragraph comment
-- * singleton list: e.g. control sequence, whitespace, letter
-- * multi-element list: group of tokens
--
-- If you need a non-empty return value, use 'nextTokenNoExpand'.
tokenNoExpand :: Monad m => LexerT m [Token]
tokenNoExpand = anyWhite <|> groupNoExpand <|> count 1
  (ctrlseqNoExpand <|> param <|> someChar)

-- | Parse a /logical unit/ of tokens without expanding them.
--
-- Like 'tokenNoExpand' but never returns the empty list.
nextTokenNoExpand :: Monad m => LexerT m [Token]
nextTokenNoExpand = parseUntilNonEmpty tokenNoExpand

-- | Parse many logical units of tokens without expanding them.
-- (See 'tokenNoExpand'.)
tokensNoExpand :: Monad m => LexerT m [Token]
tokensNoExpand = concat <$> many tokenNoExpand

-- Parse a balanced TeX group as a flat token list including delimiters.
groupNoExpand :: Monad m => LexerT m [Token]
groupNoExpand = (fmap (++) . (:)) <$> bgroup <*> tokensNoExpand <*> count 1 egroup

-- | Parse tokens (without expansion) until you hit the specified delimiter.
-- The delimiter is not included in the result.
untilTok :: Monad m => Token -> LexerT m [Token]
untilTok t = concat <$> manyTill tokenNoExpand (tok t)

-- | Parse tokens (without expansion) until you hit the specified token sequence.
-- The delimiting sequence is not included in the result.
untilToks :: Monad m => [Token] -> LexerT m [Token]
untilToks ts = concat <$> manyTill tokenNoExpand (try (mapM_ tok ts))

-- | Parse a balanced group of tokens (without expansion)
-- between the provided opening and closing tokens.
-- The (outermost) delimiters are not included in the result.
balanced :: Monad m => Token -> Token -> LexerT m [Token]
balanced open close =
  tok open *> balancedEnd open close

-- Helper function for 'balanced'. Skips delimiters.
balancedEnd :: Monad m => Token -> Token -> LexerT m [Token]
balancedEnd open close =
  ([] <$ tok close) <|>
  ((++) <$> (balancedInner open close <|> tokenNoExpand)
   <*> balancedEnd open close)

-- Helper function for 'balanced'. Keeps delimiters.
balancedInner :: Monad m => Token -> Token -> LexerT m [Token]
balancedInner open close =
  (:) <$> tok open <*> balancedInnerEnd open close

-- Helper function for 'balanced'. Keeps delimiters.
balancedInnerEnd :: Monad m => Token -> Token -> LexerT m [Token]
balancedInnerEnd open close =
  count 1 (tok close) <|>
  ((++) <$> (balancedInner open close <|> tokenNoExpand)
   <*> balancedInnerEnd open close)

-- | Parse a balanced group of tokens (without expansion)
-- between tokens with the provided opening and closing catcodes.
-- The (outermost) delimiters are not included in the result.
balancedCC :: Monad m => Catcode -> Catcode -> LexerT m [Token]
balancedCC open close =
  charcc open *> balancedCCEnd open close

-- Helper function for 'balancedCC'. Skips delimiters.
balancedCCEnd :: Monad m => Catcode -> Catcode -> LexerT m [Token]
balancedCCEnd open close =
  ([] <$ charcc close) <|>
  ((++) <$> (balancedCCInner open close <|> tokenNoExpand)
   <*> balancedCCEnd open close)

-- Helper function for 'balancedCC'. Keeps delimiters.
balancedCCInner :: Monad m => Catcode -> Catcode -> LexerT m [Token]
balancedCCInner open close =
  (:) <$> charcc open <*> balancedCCInnerEnd open close

-- Helper function for 'balancedCC'. Keeps delimiters.
balancedCCInnerEnd :: Monad m => Catcode -> Catcode -> LexerT m [Token]
balancedCCInnerEnd open close =
  count 1 (charcc close) <|>
  ((++) <$> (balancedCCInner open close <|> tokenNoExpand)
   <*> balancedCCInnerEnd open close)

-------------------- Combinators

-- | Apply a parser in a TeX group (between 'Bgroup' and 'Egroup').
-- The delimiters are not included in the result.
grouped :: Monad m => LexerT m a -> LexerT m a
grouped = between bgroup egroup

-- | Apply a parser in a TeX group (see 'grouped') or directly.
optGrouped :: Monad m => LexerT m a -> LexerT m a
optGrouped p = grouped p <|> p

-- | Apply a parser between a pair of square brackets.
-- The delimiters are not included in the result.
bracketed :: Monad m => LexerT m a -> LexerT m a
bracketed = between (char '[' Other) (char ']' Other)

-- | Apply a list parser until the result is non-empty.
parseUntilNonEmpty :: Monad m => LexerT m [Token] -> LexerT m [Token]
parseUntilNonEmpty p = p >>= \xs ->
  if null xs then parseUntilNonEmpty p else return xs

-------------------- Whitespace parsers

-- | Parse whitespace-like tokens: comments, space, eol, par.
--
-- Returns the empty list or a single whitespace token ('spcTok' or 'parTok').
-- A single application will consume all leading whitespace.
--
-- Intra-paragraph whitespace is represented by 'spcTok'.
-- Inter-paragraph whitespace is represented by 'parTok'.
anyWhite :: Monad m => LexerT m [Token]
anyWhite = comment <|> space <|> eol <|> parT

-- Parse a Comment.
-- Returns [], or [parTok] if followed by an empty line.
comment :: Monad m => LexerT m [Token]
comment = skipSingleComment *> skipOptSpacesAndComments *>
  option [] (emptyLine <|> parT)

-- Parse a Space token.
-- Returns [spcTok], or [parTok] if followed by an empty line.
space :: Monad m => LexerT m [Token]
space = charcc Space *> skipOptSpacesAndComments *>
  option [spcTok] (eol <|> parT)

-- Parse an Eol token.
-- Skips leading space on the following line.
-- Returns [spcTok], or [parTok] if followed by an empty line.
eol :: Monad m => LexerT m [Token]
eol = charcc Eol *> skipOptSpacesAndComments *>
  option [spcTok] (emptyLine <|> parT)

-- If this parser succeeds after a previously seen line break,
-- we hit an empty line and thus return [parTok].
-- This also skips any subsequent whitespace.
emptyLine :: Monad m => LexerT m [Token]
emptyLine = [parTok] <$ (charcc Eol <* skipOptWhite)

-- Parse a Par token.
-- This also skips any subsequent whitespace.
parT :: Monad m => LexerT m [Token]
parT = count 1 (tokT parTok) <* skipOptWhite

-- Skip optional spaces (no eol or par) and comments.
skipOptSpacesAndComments :: Monad m => LexerT m ()
skipOptSpacesAndComments = void . many $
  void (charcc Space) <|> skipSingleComment

-- | Skip optional 'Space' chars.
skipOptSpace :: Monad m => LexerT m ()
skipOptSpace = void . many $ charcc Space

-- | Skip all whitespace, intra-paragraph and inter-paragraph alike.
--
-- In particular, skip 'Space' and 'Eol' chars, Par tokens and comments.
-- This is typically used after encountering a paragraph break.
skipOptWhite :: Monad m => LexerT m ()
skipOptWhite = void . many $
  skipSingleComment <|> void (charcc Space <|> charcc Eol <|> tokT parTok)

-- Skip a single comment, including the trailing 'Eol' and any leading
-- space on the following line.
skipSingleComment :: Monad m => LexerT m ()
skipSingleComment = charcc Comment *> many (charccno Eol) *>
  ((charcc Eol *> skipOptSpace) <|> lookAhead eof)

-- | Skip optional intra-paragraph whitespace.
--
-- However, on encountering a paragraph break,
-- push a Par token back into the stream.
skipSpaceExceptPar :: Monad m => LexerT m ()
skipSpaceExceptPar = optional (anyWhite >>= injectParTok)
  where
    injectParTok xs@(w:_) = when (w == parTok) (prependTokens xs)
    injectParTok _ = return ()

-------------------- Unit parsers

-- | Parse an optional equals sign.
equals :: Monad m => LexerT m ()
equals = skipOptSpace *> optional (char '=' Other) *> skipOptSpace

-------------------- Char parsers

-- | Parse a raw character, ignoring category codes.
-- This will deconstruct pre-parsed tokens to raw strings.
rawChar :: Monad m => LexerT m Char
rawChar = satisfyChar (const True) <|>
  (detokenize <$> satisfyToken (const True) >>= getFirst)
  where
    getFirst (c:cs) = prependString cs *> return c
    getFirst [] = error "token was deconstructed to empty string"

-- | Parse the provided string.
--
-- This will only accept characters with catcode 'Letter'.
string :: Monad m => String -> LexerT m String
string = foldr op (return "")
  where op c = (<*>) ((:) <$> plainLetter c)
        plainLetter c = c <$ letter c

-- | Parse a valid filename.
filename :: Monad m => LexerT m String
filename =
  optGrouped (strip (many1
    (satisfyCharCC isFilenameChar filenameCCs
     <?> "valid filename character")))
  where
    strip p = skipOptSpace *> p <* skipOptSpace
    isFilenameChar x = not (isSpace x || isControl x)
    filenameCCs = [Letter, Other, Mathshift, AlignTab,
                   ParamPrefix, Supscript, Subscript, Active]

-- | Parse a decimal digit.
digit :: Monad m => LexerT m Char
digit = satisfyCharCC isDigit [Other]

-- | Parse an octal digit.
octDigit :: Monad m => LexerT m Char
octDigit = satisfyCharCC isOctDigit [Other]

-- | Parse a hexadecimal digit.
--
-- Note: TeX wants cc(0-9) = Other, cc(a-f)= Other|Letter.
-- We are more liberal and only require cc(0-9a-f) = Other|Letter.
hexDigit :: Monad m => LexerT m Char
hexDigit = satisfyCharCC isHexDigit [Other, Letter]

-------------------- Number parsers

-- | Parse a number.
number :: Monad m => LexerT m Int
number = choice [readcc, chrnum, hexnum, octnum, decnum]

-- Parse an alpha constant.
chrnum :: Monad m => LexerT m Int
chrnum = (fromEnum . getRawChar) <$> (tok ordPrefix *> anyCharOptEsc)

-- Parse a hex constant.
hexnum :: Monad m => LexerT m Int
hexnum = hexToInt <$> (tok hexPrefix *> many1 hexDigit)

-- Parse an octal constant.
octnum :: Monad m => LexerT m Int
octnum = octToInt <$> (tok octPrefix *> many1 octDigit)

-- Parse a decimal constant.
decnum :: Monad m => LexerT m Int
decnum = decToInt <$> many1 digit

-- | Parse a single decimal digit.
singleDigit :: Monad m => LexerT m Int
singleDigit = decToInt <$> count 1 digit

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
readcc :: Monad m => LexerT m Int
readcc = do
  cctab <- getCatcodes <$> getState
  ch <- toEnum <$> (ctrlseqEq "catcode" False *> number)
  return $ fromEnum (catcodeOf ch cctab)
