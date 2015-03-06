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
    parseTeX
  , replParseTeX
  ) where

import Control.Applicative ((<*), (<*>), (*>), (<$), (<$>))
import Control.Monad (void)
import Data.Char (isOctDigit, isDigit, isHexDigit)
import Numeric (readOct, readDec, readHex)
import Text.Parsec
  (Parsec, tokenPrim, runParser,
   getState, putState, modifyState, getInput, setInput,
   (<|>), many, many1, manyTill, choice, optional, count, eof,
   (<?>), unexpected)
import Text.Parsec.Pos (updatePosChar)

import Text.TeX.Lexer.Catcode
  (Catcode(..), catcodeOf, hasCatcode,
   CatcodeTable, defaultCatcodeTable, updateCatcodeTable,
   catcodesAllowed, catcodesPassive, catcodesNonescaped)
import Text.TeX.Lexer.Macro
  (Macro, macroContext, macroBody, applyMacro)
import Text.TeX.Lexer.Token
  (Token(..), CharOrToken, isCtrlSeq, isParam,
   isCharEq, isCharSat, hasCC, stripBraces)


-------------------- TeX constants: number prefixes

hexPrefix :: Token
hexPrefix = TeXChar '"' Other

octPrefix :: Token
octPrefix = TeXChar '\'' Other

ordPrefix :: Token
ordPrefix = TeXChar '`' Other

-------------------- Parser state

type Parser = Parsec [CharOrToken] UserState

data UserState = UserState
  { getCatcodes :: CatcodeTable
  , getMacros :: [Macro]
  , allowExpand :: Bool
  } deriving (Eq, Show)

defaultState :: UserState
defaultState = UserState
  { getCatcodes = defaultCatcodeTable
  , getMacros = []
  , allowExpand = True
  }

-- Prefix macro to the list of currently active macros.
-- The new macro will shadow others with the same name,
-- due to @lookup@'s left bias.
registerMacro :: Macro -> UserState -> UserState
registerMacro m s = let oldMacros = getMacros s
                    in s { getMacros = m:oldMacros }

-------------------- Macro expansion

expand :: Token -> Parser [Token]
expand t@(CtrlSeq name active) = do
  expandMode <- allowExpand <$> getState
  if expandMode
    then expandMacro name active
    else return [t]
expand _ = error "Trying to expand non-CtrlSeq."

expandMacro :: String -> Bool -> Parser [Token]
expandMacro name active = case (name, active) of
  ("catcode", False) -> catcode
  ("def", False) -> def
  _ -> lookupUserMacro name active

---------- Builtin macros

-- Parse the body of a @catcode@ command, execute it (by changing the
-- current catcode table) and remove catcode command from the token stream.
catcode :: Parser [Token]
catcode = do
  chCode <- number
  equals
  ccNew <- number -- Note: @toEnum@ will (correctly) fail if not in range 0-15.
  s <- getState
  let cctab = updateCatcodeTable (getCatcodes s) (toEnum chCode, toEnum ccNew)
  putState (s {getCatcodes = cctab})
  return []

-- Parse a macro definition, execute it (by updating the list of
-- active macros) and remove the @def@ command from the token stream.
def :: Parser [Token]
def = do
  -- preparation: disallow expansion of embedded macros
  s0 <- getState
  let expandMode = allowExpand s0
  putState (s0 { allowExpand = False })
  -- parse the macro definition
  (CtrlSeq name active) <- ctrlseqNoexpand <?> "macro name"
  context <- macroContextDefinition <?> "macro context definition"
  void bgroup <?> "begin of macro body"
  body <- tokens <?> "macro body"
  void egroup <?> "end of macro body"
  -- register the new macro and restore the original expansion mode
  s1 <- getState -- get current state again, it might have changed
  putState (s1 { allowExpand = expandMode })
  modifyState (registerMacro ((name, active), (context, body)))
  return []

-- Parse a macro context definition. Similar to 'pTokens', but
-- must not contain 'Bgroup' (so do not include 'pBlock' parser).
macroContextDefinition :: Parser [Token]
macroContextDefinition = concat <$> many (paramTokens <|> plainTokens)
  where
    paramTokens = count 1 param
    plainTokens = count 1 ctrlseqNoexpand
                  <|> comment
                  <|> count 1 someChar

---------- User-defined macros

-- Given the name of a control sequence, look up whether there is a
-- matching user-defined macro in the current parser state.
lookupUserMacro :: String -> Bool -> Parser [Token]
lookupUserMacro name active = do
  ms <- getMacros <$> getState
  case lookup (name, active) ms of
    Just m -> expandUserMacro ((name, active), m)
    Nothing -> return [CtrlSeq name active]

-- Expand a call of a user-defined macro.
expandUserMacro :: Macro -> Parser [Token]
expandUserMacro m = do
  -- preparation: disallow expansion of embedded macros in arguments
  s0 <- getState
  let expandMode = allowExpand s0
  putState (s0 { allowExpand = False })
  -- read arguments
  args <- macroContextInstance (macroContext m)
  -- restore expansion mode
  s1 <- getState
  putState (s1 { allowExpand = expandMode })
  -- apply macro to arguments and push the result back into the input stream
  let toks = applyMacro (macroBody m) args
  oldInput <- getInput
  setInput $ (map Right toks) ++ oldInput
  return []

-- Parse an instance of a macro context, i.e. the actual arguments of a macro call.
-- Note: The number of tokens mapped to a parameter depends on its successor.
--   * single token if followed by another 'Param' or nil, or
--   * list of tokens if followed by a literal token ('CtrlSeq' or 'TeXChar').
macroContextInstance :: [Token] -> Parser [[Token]]
macroContextInstance [] = return []
macroContextInstance [t] = case t of
  (Param _ _) -> token >>= \ts -> return [stripBraces ts]
  _ -> tok t *> return []
macroContextInstance (t1:t2:ts) = case (t1,t2) of
  ((Param _ _), (Param _ _)) -> ((:) . stripBraces) <$> token <*> macroContextInstance (t2:ts)
  ((Param _ _), _) -> ((:) . stripBraces) <$> (concat <$> manyTill token (tok t2)) <*> macroContextInstance ts
  (_,_) -> tok t1 *> macroContextInstance (t2:ts)


-------------------- Main parsers

-- This parser is applied directly to TeX documents.
mainParser :: Parser [Token]
mainParser = tokens <* eof

tokens :: Parser [Token]
tokens = concat <$> many token

-- Parse a /logical unit/ of tokens. This is either a single 'Token'
-- ('TeXChar', 'CtrlSeq', 'Param') or a block or a comment. We try to parse
-- as little structure as possible. Still we need to recognize blocks
-- because many lexer-level commands have block scope (e.g. @\catcode@).
token :: Parser [Token]
token = ctrlseq
        <|> block
        <|> comment
        <|> count 1 param
        <|> count 1 someChar

-------------------- 'TeXChar' Parsers

bgroup :: Parser Token
bgroup = charcc Bgroup

egroup :: Parser Token
egroup = charcc Egroup


-- Parse any 'TeXChar' if its catcode is in the provided list.
anyCharCC :: [Catcode] -> Parser Token
anyCharCC ccs = choice $ map charcc ccs

-- Parse a 'TeXChar' of any /passive/ catcode.
someChar :: Parser Token
someChar = anyCharCC catcodesPassive

-- Parse a 'TeXChar' of any /allowed/ catcode.
anyChar :: Parser Token
anyChar = anyCharCC catcodesAllowed

-- Parse a 'TeXChar' that is optionally prefixed by an 'Escape' char.
anyCharOptEsc :: Parser Token
anyCharOptEsc = anyCharCC catcodesNonescaped <|> anyEscapedChar

-- Parse a 'TeXChar' that is prefixed by an 'Escape' char.
-- In terms of tokens, this is a single character 'CtrlSeq'.
--
-- If we encounter a multi-character control sequence, TeX fails
-- with the error message /Improper alphabetic constant/.
-- Example:
--   \number`\ab              % error because ` cannot be applied to @(CtrlSeq "ab")@
--   \def\a{\number`\ab} \a   % error because ` cannot be applied to @(CtrlSeq "ab")@
-- By comparison, the following does work:
--   \number`\a b             % => 97b
--   \def\a{\number`\a b} \a  % => 97b
anyEscapedChar :: Parser Token
anyEscapedChar = do
  (CtrlSeq cs _) <- ctrlseqNoexpand
  if length cs == 1
    then return (TeXChar (head cs) Other) -- Note: coercing to catcode 'Other' here
    else unexpected $ cs ++ " (where a single character was required). "
         ++ "TeX: Improper alphabetic constant."

-------------------- 'CtrlSeq' Parsers

ctrlseq :: Parser [Token]
ctrlseq = ctrlseqNoexpand >>= expand

ctrlseqNoexpand :: Parser Token
ctrlseqNoexpand = ctrlseqT <|> ctrlseqC <|> activeC

ctrlseqT :: Parser Token
ctrlseqT = satisfyToken isCtrlSeq

ctrlseqC :: Parser Token
ctrlseqC = do
  -- Note: we are using plain char parsers here
  -- because parsed tokens cannot compose to a 'CtrlSeq'.
  void $ charccC Escape
  cs <- (many1 (charccC Letter) <* skipSpace)
        <|> count 1 anyChar
        <?> "control sequence"
  return (CtrlSeq (map getRawChar cs) False)

activeC :: Parser Token
activeC = charccC Active >>= \(TeXChar c _) -> return (CtrlSeq [c] True)

ctrlseqEqC :: String -> Bool -> Parser Token
ctrlseqEqC name True = let c = head name -- for active chars @length name == 1@
                       in char c Active *> return (CtrlSeq [c] True)
ctrlseqEqC name False = do
  void $ charccC Escape
  cs <- string name
  return (CtrlSeq cs False)

-------------------- 'Param' Parsers

-- Parse a TeX parameter token in a macro definition or body.
-- Typical examples: #1, #2, ##1.
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

-- Parse a TeX block as a flat token list including the delimiters.
block :: Parser [Token]
block = do
  b <- charcc Bgroup <?> "begin of group"
  -- store catcode table and macros
  s <- getState
  let cctab = getCatcodes s
      activeMacros = getMacros s
  toks <- tokens <?> "group body"
  e <- charcc Egroup <?> "end of group"
  -- restore catcode table and macros
  putState (s { getCatcodes = cctab
              , getMacros = activeMacros })
  return ([b] ++ toks ++ [e])

-- All chars between 'Comment' and 'Eol' are treated as literal chars,
-- including 'Escape' characters (e.g. we must not interpret @\catcode@
-- commands within comments).
comment :: Parser [Token]
comment = (:) <$> charcc Comment <*>
          ((++) <$> many (charccno Eol) <*>
           count 1 (charcc Eol))

-------------------- Unit parsers

skipSpace :: Parser ()
skipSpace = many (charcc Space) *> return ()

equals :: Parser ()
equals = skipSpace *> optional (char '=' Other) *> skipSpace

-------------------- Number parsers

number :: Parser Int
number = choice [chrnum, hexnum, octnum, decnum]

chrnum :: Parser Int
chrnum = (fromEnum . getRawChar) <$> (tok ordPrefix *> anyCharOptEsc)

hexnum :: Parser Int
hexnum = hexToInt <$> (tok hexPrefix *> many1 hexDigit)

octnum :: Parser Int
octnum = octToInt <$> (tok octPrefix *> many1 octDigit)

decnum :: Parser Int
decnum = decToInt <$> many1 digit

decToInt :: String -> Int
decToInt cs = case readDec cs of
  [(i,[])] -> i
  _ -> error $ "Cannot read dec number: " ++ cs

hexToInt :: String -> Int
hexToInt cs = case readHex cs of
  [(i,[])] -> i
  _ -> error $ "Cannot read hex number: " ++ cs

octToInt :: String -> Int
octToInt cs = case readOct cs of
  [(i,[])] -> i
  _ -> error $ "Cannot read oct number: " ++ cs


-------------------- Char parsers: adapt "Text.Parsec.Char" for TeX characters

-- Parse the provided character if it has the provided catcode.
char :: Char -> Catcode -> Parser Token
char c i = charT c i <|> charC c i

-- @char@ for tokens.
charT :: Char -> Catcode -> Parser Token
charT c i = satisfyToken (isCharEq c i)

-- @char@ for plain characters.
charC :: Char -> Catcode -> Parser Token
charC c i = do
  cctab <- getCatcodes <$> getState
  void $ satisfyChar (\x -> (x==c && hasCatcode i cctab x))
  return (TeXChar c i)

-- 'string' only accepts letters, i.e. characters with catcode 'Letter'.
string :: String -> Parser String
string = foldr op (return "")
  where op = \c -> (<*>) ((:) <$> (plainLetter c))
        plainLetter = \c -> (c <$ char c Letter)

-- Octal digits.
octDigit :: Parser Char
octDigit = charSatCCs isOctDigit [Other]

-- Decimal digits (think @decDigit@).
digit :: Parser Char
digit = charSatCCs isDigit [Other]

-- Hexadecimal digits.
--
-- Note: TeX wants cc(0-9) = Other, cc(a-f)= Other|Letter.
-- We are more liberal and only require cc(0-9a-f) = Other|Letter.
hexDigit :: Parser Char
hexDigit = charSatCCs isHexDigit [Other, Letter]

-------------------- Basic parsers (low-level)

-- Parse the provided token.
tok :: Token -> Parser Token
tok t = tokT t <|> tokC t

-- @tok@ for tokens.
tokT :: Token -> Parser Token
tokT t = satisfyToken (== t)

-- @tok@ for plain characters.
tokC :: Token -> Parser Token
tokC (TeXChar c i) = charC c i
tokC (CtrlSeq cs active) = ctrlseqEqC cs active
tokC t@(Param i n) = count n (charccC ParamPrefix)
                     *> charC (head (show i)) Other -- assert: 1 <= i <= 9
                     *> return t

-- Parse a character that satisfies property @p@ and
-- has one of the catcodes in @ccs@.
charSatCCs :: (Char -> Bool) -> [Catcode] -> Parser Char
charSatCCs p ccs = charSatCCsT p ccs <|> charSatCCsC p ccs

charSatCCsT :: (Char -> Bool) -> [Catcode] -> Parser Char
charSatCCsT p ccs = getRawChar <$> satisfyToken
                    (\t -> isCharSat p t && any (`hasCC` t) ccs)

charSatCCsC :: (Char -> Bool) -> [Catcode] -> Parser Char
charSatCCsC p ccs = do
  cctab <- getCatcodes <$> getState
  satisfyChar (\x -> p x && any (\cc -> hasCatcode cc cctab x) ccs)

-- Parse a character with the provided catcode.
--
-- This can be either an already-parsed 'TeXChar' with a stored catcode,
-- or a yet unseen plain character (whose catcode matches, according
-- to the current catcode table).
--
-- Note: This can also be expressed in terms of @charSatCCs@ as follows:
-- charcc i = charSatCCs (const True) [i] >>= \c -> return (TeXChar c i)
charcc :: Catcode -> Parser Token
charcc i = charccT i <|> charccC i

-- Parse a character that has not the provided catcode.
charccno :: Catcode -> Parser Token
charccno i = charccTno i <|> charccCno i

-- Parse a 'TeXChar' with the provided catcode.
charccT :: Catcode -> Parser Token
charccT i = satisfyToken (hasCC i)

-- Parse a 'TeXChar' that has not the provided catcode.
charccTno :: Catcode -> Parser Token
charccTno i = satisfyToken (not . hasCC i)

-- Parse a plain character if it fits the provided catcode,
-- and return it as a 'TeXChar'.
charccC :: Catcode -> Parser Token
charccC i = do
  cctab <- getCatcodes <$> getState
  c <- satisfyChar (hasCatcode i cctab)
  return (TeXChar c i)

-- Parse a plain character that has not the provided catcode,
-- and return it as a 'TeXChar'.
charccCno :: Catcode -> Parser Token
charccCno i = do
  cctab <- getCatcodes <$> getState
  c <- satisfyChar (not . hasCatcode i cctab)
  return (TeXChar c (catcodeOf c cctab))


-- Parse a @Right@ stream element.
satisfyToken :: (Token -> Bool) -> Parser Token
satisfyToken p = satisfy (either (const False) p) >>= \(Right t) -> return t

-- Parse a @Left@ stream element.
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


-------------------- main: run lexer

-- | Run TeX lexer on named input string and return a 'Token' list.
-- (The resulting 'Token' stream is restricted to 'TeXChar' and
-- 'CtrlSeq' elements, i.e. no 'Param' elements.) Exit on failure.
parseTeX :: String -> String -> [Token]
parseTeX name input = case runParser mainParser defaultState name (map Left input) of
  Left l  -> error (show l)
  Right r -> r

-- | Run TeX lexer on input string and return all results and errors as strings.
replParseTeX :: String -> String
replParseTeX input = case runParser mainParser defaultState "" (map Left input) of
  Left err  -> "ERROR " ++ show err
  Right ans -> show ans

{-
-- For testing parsers. Run the provided parser on the given input.
-- Examples:
-- >>> parseWith number [Left '`', Left 'a']                            -- Right 97
-- >>> parseWith number [Right (TeXChar '`' 12), Right (CtrlSeq "a")]   -- Right 97
-- >>> parseWith number [Right (TeXChar '`' 12), Right (CtrlSeq "ab")]  -- Left ParserError
parseWith :: Parser a -> [CharOrToken] -> Either ParseError a
parseWith parser input = runParser parser defaultState "" input
-}
