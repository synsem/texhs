{-# LANGUAGE CPP #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.TeX.Lexer.TokenParser.Execution
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  GHC
--
-- Expanding parsers for TeX tokens and execution of TeX primitives.
----------------------------------------------------------------------

module Text.TeX.Lexer.TokenParser.Execution
  ( -- * Expanding token parsers
    texLexer
  ) where

#if MIN_VERSION_base(4,8,0)
-- Prelude exports all required operators from Control.Applicative
#else
import Control.Applicative ((<*), (<*>), (*>), (<$), (<$>))
#endif
import Control.Monad ((>=>))
import Data.Maybe (fromMaybe)

import Text.TeX.Lexer.Catcode
import Text.TeX.Lexer.Macro
import Text.TeX.Lexer.Token
import Text.TeX.Lexer.TokenParser.Basic
import Text.TeX.Lexer.TokenParser.Core
import Text.TeX.Lexer.TokenParser.Expansion
import Text.TeX.Lexer.TokenParser.State


---------------------------------------- Expanding token parsers

---------- Main document parser

-- | TeX Lexer: Convert TeX source document to a 'Token' stream.
texLexer :: HandleTeXIO m => LexerT m [Token]
texLexer = optional anyWhite *> tokens <* eof

---------- Multi-token parsers with expansion

-- Parse many tokens using 'token'.
tokens :: HandleTeXIO m => LexerT m [Token]
tokens = concat <$> many token

-- Parse a /logical unit/ of tokens. This is either a single 'Token'
-- ('TeXChar', 'CtrlSeq', 'Param') or a group or a possibly empty list
-- of tokens resulting from macro expansion. We try to parse as little
-- structure as possible. Still we need to recognize groups because
-- many lexer-level commands have group scope (e.g. @\\catcode@).
token :: HandleTeXIO m => LexerT m [Token]
token = anyWhite <|> group <|> ctrlseq <|>
        count 1 (param <|> someChar)

-- Parse a balanced TeX group as a flat token list including delimiters.
group :: HandleTeXIO m => LexerT m [Token]
group = (fmap (++) . (:)) <$> bgroup <*> tokens <*> count 1 egroup

-- Parse a control sequence and try to expand or execute it.
-- If no definition is known, return the token unmodified.
ctrlseq :: HandleTeXIO m => LexerT m [Token]
ctrlseq = do
  t@(CtrlSeq name active) <- ctrlseqNoExpand
  st <- getState
  case lookupMacroCmd (name, active) st of
    Just m@(MacroCmdUser{}) -> [] <$ expand m
    Just (MacroCmdPrim p)
      | p `elem` primitiveConstants -> return [t]
      | otherwise -> executePrimitive p
    Just (MacroCmdChar ch cc) -> [] <$ prependTokens [TeXChar ch cc]
    Nothing -> return [t]

---------------------------------------- Execute TeX primitives

-- | Meanings of primitives.
--
-- These internal executable commands cannot be changed by the user.
primitiveMeanings :: HandleTeXIO m => [(Primitive, LexerT m [Token])]
primitiveMeanings =
  [ ("begingroup", [bgroupTok] <$ modifyState (pushGroup NativeGroup))
  , ("endgroup", [egroupTok] <$ modifyState (popGroup NativeGroup))
  , ("bgroup", [bgroupTok] <$ modifyState (pushGroup AnonymousGroup))
  , ("egroup", [egroupTok] <$ modifyState (popGroup AnonymousGroup))
  , ("(", return [mathTok])
  , (")", return [mathTok])
  , ("[", return (replicate 2 mathTok))
  , ("]", return (replicate 2 mathTok))
  , ("begin", beginEnvironment)
  , ("end", endEnvironment)
  , ("catcode", catcode)
  , ("def", def)
  , ("let", letmeaning)
  , ("iftrue", iftrue)
  , ("iffalse", iffalse)
  , ("char", count 1 chr)
  , ("number", numbertoks)
  , ("NewDocumentCommand", declareDocumentCommand MacroNew)
  , ("RenewDocumentCommand", declareDocumentCommand MacroRenew)
  , ("ProvideDocumentCommand", declareDocumentCommand MacroProvide)
  , ("DeclareDocumentCommand", declareDocumentCommand MacroDeclare)
  , ("NewDocumentEnvironment", declareDocumentEnvironment MacroNew)
  , ("RenewDocumentEnvironment", declareDocumentEnvironment MacroRenew)
  , ("ProvideDocumentEnvironment", declareDocumentEnvironment MacroProvide)
  , ("DeclareDocumentEnvironment", declareDocumentEnvironment MacroDeclare)
  , ("IfBooleanTF", xparseif trueTok)
  , ("IfNoValueTF", xparseif noValueTok)
  , ("newcommand", newcommand MacroNew)
  , ("renewcommand", newcommand MacroRenew)
  , ("providecommand", newcommand MacroProvide)
  , ("DeclareRobustCommand", newcommand MacroDeclare)
  , ("newenvironment", newenvironment MacroNew)
  , ("renewenvironment", newenvironment MacroRenew)
  , ("input", readInputFile)
  , ("include", readInputFile)
  , ("date", mkString <$> handleReadDate)
  , ("meaning", meaning)
  , ("undefined", error "undefined control sequence")
  ]

-- Primitive constants have no direct implementation,
-- they are used as constants in restricted contexts.
primitiveConstants :: [Primitive]
primitiveConstants = ["else", "fi"]

-- A list of primitives that start a new conditional.
conditionalHeads :: [Primitive]
conditionalHeads = ["iftrue", "iffalse"]

-- | Execute a primitive command.
executePrimitive :: HandleTeXIO m => Primitive -> LexerT m [Token]
executePrimitive name = fromMaybe throwError $ lookup name primitiveMeanings
  where
    throwError = error $ "Call to undefined primitive: " ++ name

-------------------- Handle builtin macros

---------- Builtin macros: file input

readInputFile :: HandleTeXIO m => LexerT m [Token]
readInputFile = [] <$ (filename >>= (handleReadFile >=> prependString))

---------- Builtin macros: meaning

-- Assign new meaning to a control sequence.
letmeaning :: Monad m => LexerT m [Token]
letmeaning = do
  (CtrlSeq name active) <- ctrlseqNoExpand <?> "macro name"
  equals
  m <- meaningCtrlSeq <|> meaningChar
  let val = case m of
        MeaningChar ch cc -> MacroCmdChar ch cc
        MeaningMacro cmd -> cmd
        MeaningUndef -> MacroCmdPrim "undefined"
  let key = (name, active)
  modifyState (registerLocalMacroCmd (key, val))
  return []

-- Show the current meaning of a control sequence or a character.
meaning :: Monad m => LexerT m [Token]
meaning = (mkQuote . showMeaning) <$> (meaningCtrlSeq <|> meaningChar)

-- Parse a control sequence and return its meaning.
meaningCtrlSeq :: Monad m => LexerT m Meaning
meaningCtrlSeq = do
  (CtrlSeq name active) <- ctrlseqNoExpand
  st <- getState
  return (getMacroMeaning st (name, active))

-- Parse a single raw character (catcode-independent) and return its meaning.
meaningChar :: Monad m => LexerT m Meaning
meaningChar = getCharMeaning <$> getState <*> rawChar

---------- Builtin macros: numbers

-- Parse a character by its number. We are treating these characters
-- as Tokens with catcode 'Other'.
chr :: Monad m => LexerT m Token
chr = number >>= \chCode -> return (TeXChar (toEnum chCode) Other)

-- Convert an internal integer to its string representation.
numbertoks :: Monad m => LexerT m [Token]
numbertoks = (map (`TeXChar` Other) . show) <$> number

---------- Builtin macros: conditionals

-- Expand a conditional to its left branch.
iftrue :: HandleTeXIO m => LexerT m [Token]
iftrue = [] <$ conditionalPush True

-- Expand a conditional to its right branch.
iffalse :: HandleTeXIO m => LexerT m [Token]
iffalse = [] <$ conditionalPush False

-- Expand a conditional and push the resulting token list
-- back into the stream (for group detection).
conditionalPush :: HandleTeXIO m => Bool -> LexerT m ()
conditionalPush = conditional >=> prependTokens

-- Parse a conditional and return its left or right branch,
-- depending on the provided flag.
--
-- The flag argument indicates whether the condition is true.
-- Note: Conditional nesting is independent of grouping,
-- so groups are not parsed here.
conditional :: HandleTeXIO m => Bool -> LexerT m [Token]
conditional b = do
  let (l,r) = (b, not b) -- branch expansion modes
  (leftToks, rightToks) <- condBranches (l,r) []
  return $ if b then leftToks else rightToks

-- Parse the two branches of a conditional.
--
-- The flag arguments indicate whether to expand the branches.
condBranches :: HandleTeXIO m => (Bool, Bool) -> [Token] -> LexerT m ([Token], [Token])
condBranches (expandLeft,expandRight) ltoks = do
  t <- tokenCond expandLeft
  s <- getState
  let means = meansPrimitive s
  case t of
    [CtrlSeq n a]
      | (n, a) `means` "fi" ->
          return (ltoks, [])
      | (n, a) `means` "else" ->
          (,) ltoks <$> condRightBranch expandRight []
      | any ((n, a) `means`) conditionalHeads ->
          -- handle embedded conditional in dead branch
          condBranches (False, False) [] *>
            condBranches (expandLeft, expandRight) ltoks
      | otherwise ->
          condBranches (expandLeft, expandRight) (ltoks ++ t)
    _ -> condBranches (expandLeft, expandRight) (ltoks ++ t)

-- Parse the second branch of a conditional.
--
-- The flag argument indicates whether to expand the branch.
condRightBranch :: HandleTeXIO m => Bool -> [Token] -> LexerT m [Token]
condRightBranch expandMode toks = do
  t <- tokenCond expandMode
  s <- getState
  let means = meansPrimitive s
  case t of
    [CtrlSeq n a]
      | (n, a) `means` "fi" ->
          return toks
      | any ((n, a) `means`) conditionalHeads ->
        -- handle embedded conditional in dead branch
          condBranches (False, False) [] *>
            condRightBranch expandMode toks
      | otherwise ->
          condRightBranch expandMode (toks ++ t)
    _ -> condRightBranch expandMode (toks ++ t)

-- Parse a token in a conditional.
--
-- Note: Grouping characters are parsed literally.
tokenCond :: HandleTeXIO m => Bool -> LexerT m [Token]
tokenCond expandMode = parseUntilNonEmpty $ anyWhite <|>
  (if expandMode then ctrlseq else count 1 ctrlseqNoExpand) <|>
  count 1 (charcc Bgroup <|> charcc Egroup <|> param <|> someChar)

-- Evaluate an xparse-style conditional.
--
-- Note: In LaTeX3 this is defined via @\\ifx@.
xparseif :: HandleTeXIO m => Token -> LexerT m [Token]
xparseif t = do
  rs <- token <* skipOptSpace
  stripBraces <$> case stripBraces rs of
    [r] | r == t -> token <* skipOptSpace <* token
    _ -> token *> skipOptSpace *> token

---------- Builtin macros: catcodes

-- Parse the body of a @catcode@ command, execute it (by changing the
-- current catcode table) and remove catcode command from the token stream.
catcode :: Monad m => LexerT m [Token]
catcode = do
  chCode <- number
  equals
  ccNew <- number -- Note: @toEnum@ will fail if not in range 0-15.
  modifyState (addCatcode (toEnum chCode, toEnum ccNew))
  return []

---------- Builtin macros: TeX macro definitions

-- Parse a macro definition, execute it (by updating the list of
-- active macros) and remove the @def@ command from the token stream.
def :: Monad m => LexerT m [Token]
def = do
  (CtrlSeq name active) <- ctrlseqNoExpand <?> "macro name"
  context <- macroContextDefinition <?> "macro context definition"
  body <- grouped tokensNoExpand
  let key = (name, active)
  modifyState (registerLocalMacroCmd
               (key, MacroCmdUser key (def2xparse context) body))
  return []

-- Parse a macro context definition. Similar to 'tokens', but
-- must not contain 'Bgroup' (so do not include 'group' parser).
macroContextDefinition :: Monad m => LexerT m [Token]
macroContextDefinition =
  concat <$> many (anyWhite <|> count 1
                   (param <|> ctrlseqNoExpand <|> someChar))

-- Convert def-style macro context to an xparse argspec.
--
-- The number of tokens mapped to a parameter depends on its successor.
--   * single token if followed by another 'Param' or nil, or
--   * list of tokens if followed by a literal token ('CtrlSeq' or 'TeXChar').
def2xparse :: [Token] -> ArgSpec
def2xparse [] = []
def2xparse [Param _ _] = [Mandatory]
def2xparse [t] = [LiteralToken t]
def2xparse (Param _ _:ts@(Param _ _:_)) = Mandatory : def2xparse ts
def2xparse (Param _ _:t2:ts) = Until [t2] : def2xparse ts
def2xparse (t1:ts@(_:_)) = LiteralToken t1 : def2xparse ts

---------- Builtin macros: LaTeX3 (xparse) macro definitions

-- Parse and register an xparse macro definition.
declareDocumentCommand :: HandleTeXIO m => MacroDefinitionMode -> LexerT m [Token]
declareDocumentCommand defMode = do
  (CtrlSeq name active) <- optGrouped ctrlseqNoExpand <?> "macro name"
  context <- argspec <?> "macro argspec"
  body <- grouped tokensNoExpand
  let key = (name, active)
  modifyState $ registerMacroCmd defMode
    (key, MacroCmdUser key context body)
  return []

-- Parse and register an xparse environment definition.
declareDocumentEnvironment :: HandleTeXIO m => MacroDefinitionMode -> LexerT m [Token]
declareDocumentEnvironment defMode = do
  name <- grouped tokensNoExpand <?> "environment name"
  context <- argspec <?> "environment argspec"
  startCode <- grouped tokensNoExpand <?> "environment start code"
  endCode <- grouped tokensNoExpand <?> "environment end code"
  modifyState $ registerMacroEnv defMode
    (name, MacroEnv name context startCode endCode)
  return []

-- Parse a full xparse-style argument specification.
argspec :: HandleTeXIO m => LexerT m ArgSpec
argspec = grouped (skipOptSpace *> many argtype) <* skipOptSpace

-- Parse a single xparse-style argument type.
--
-- Not implemented: 'v' (verbatim), '>' (argument processor).
argtype :: HandleTeXIO m => LexerT m ArgType
argtype = optional (char '+' Other) *> choice
          [ Mandatory <$ letter 'm'
          , Until <$>
            (letter 'u' *>
             (grouped tokens <|> count 1 singleToken))
          , UntilCC Bgroup <$ letter 'l'
          , Delimited <$>
            (letter 'r' *> singleToken) <*>
            singleToken <*> return Nothing
          , Delimited <$>
            (letter 'R' *> singleToken) <*>
            singleToken <*> (Just <$> grouped tokens)
          , OptionalGroup
            (mkOther '[') (mkOther ']')
            Nothing <$ letter 'o'
          , OptionalGroup <$>
            (letter 'd' *> singleToken) <*>
            singleToken <*> return Nothing
          , OptionalGroup
            (mkOther '[') (mkOther ']')
            <$> (letter 'O' *> (Just <$> grouped tokens))
          , OptionalGroup <$>
            (letter 'D' *> singleToken) <*>
            singleToken <*> (Just <$> grouped tokens)
          , OptionalGroupCC Bgroup Egroup Nothing <$
            letter 'g'
          , OptionalGroupCC Bgroup Egroup <$>
            (letter 'G' *> (Just <$> grouped tokens))
          , OptionalToken
            (mkOther '*') <$ letter 's'
          , OptionalToken
            <$> (letter 't' *> singleToken)
          ] <* skipOptSpace

---------- Builtin macros: LaTeX2e macro definitions

-- Parse and register a LaTeX2e macro definition.
newcommand :: Monad m => MacroDefinitionMode -> LexerT m [Token]
newcommand defMode = do
  optional (char '*' Other) -- ignore 'long' property
  (CtrlSeq name active) <- optGrouped ctrlseqNoExpand <?> "macro name"
  context <- latexMacroContext
  body <- grouped tokensNoExpand <|> count 1 singleToken
  let key = (name, active)
  modifyState $ registerMacroCmd defMode
    (key, MacroCmdUser key context body)
  return []

-- Parse and register a LaTeX2e environment definition.
newenvironment :: HandleTeXIO m => MacroDefinitionMode -> LexerT m [Token]
newenvironment defMode = do
  name <- grouped tokens <?> "environment name"
  context <- latexMacroContext
  startCode <- grouped tokensNoExpand <?> "environment start code"
  endCode <- grouped tokensNoExpand <?> "environment end code"
  modifyState $ registerMacroEnv defMode
    (name, MacroEnv name context startCode endCode)
  return []

-- Parse a LaTeX2e macro context definition and
-- convert it to an xparse-style ArgSpec.
latexMacroContext :: Monad m => LexerT m ArgSpec
latexMacroContext = do
  numArgs <- option 0 (bracketed singleDigit)
  let open = mkOther '['
      close = mkOther ']'
  optArg <- optionMaybe (balanced open close)
  let context = case optArg of
        Just d -> OptionalGroup open close (Just d) :
                  replicate (numArgs-1) Mandatory
        Nothing -> replicate numArgs Mandatory
  return context

-------------------- Handle LaTeX environments (named groups)

-- Start TeX group and try to expand user-defined environment definitions.
beginEnvironment :: HandleTeXIO m => LexerT m [Token]
beginEnvironment = do
  name <- envName
  -- Note: expansion must be enabled because we are expanding 'begin'
  st <- getState
  case lookupMacroEnv (stripBraces name) st of
    Nothing -> let grp = NamedGroup (stripBraces name)
               in modifyState (pushGroup grp) *>
                  return (mkCtrlSeq "begin": name)
    Just envdef -> do
      (startCode, endCode) <- expandEnvironment envdef
      modifyState . pushGroup $
        DefinedGroup (stripBraces name) startCode endCode
      prependTokens startCode
      return []

-- Close matching TeX group and inject end code for user-defined environments.
endEnvironment :: HandleTeXIO m => LexerT m [Token]
endEnvironment = do
  name <- envName
  let endEnv = mkCtrlSeq "end": name
  grp <- getGroup <$> getState
  case grp of
    (DefinedGroup name' _ endCode) ->
      if null endCode
      then do -- close group
        modifyState . popGroup $
          DefinedGroup (stripBraces name) [] []
        return []
      else do -- inject end code
        modifyState . setGroup $
          DefinedGroup name' [] []
        prependTokens (endCode ++ endEnv)
        return []
    _ -> do
      modifyState (popGroup (NamedGroup (stripBraces name)))
      return endEnv

-- Parse the name of a LaTeX environment, including group delimiters.
--
-- We currently allow arbitrary token lists.
envName :: HandleTeXIO m => LexerT m [Token]
envName = group
