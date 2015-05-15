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
-- Portability :  portable
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
import Text.Parsec
  (getState, modifyState, (<|>), many, choice,
   option, optionMaybe, optional, count, eof, (<?>))

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
texLexer :: Parser [Token]
texLexer = skipOptWhite *> tokens <* eof

---------- Multi-token parsers with expansion

-- Parse many tokens using 'token'.
tokens :: Parser [Token]
tokens = concat <$> many token

-- Parse a /logical unit/ of tokens. This is either a single 'Token'
-- ('TeXChar', 'CtrlSeq', 'Param') or a group or a possibly empty list
-- of tokens resulting from macro expansion. We try to parse as little
-- structure as possible. Still we need to recognize groups because
-- many lexer-level commands have group scope (e.g. @\\catcode@).
token :: Parser [Token]
token = skipOptCommentsPAR *>
        (group <|> ctrlseq <|> count 1
         (eolpar <|> param <|> someChar))

-- Parse a balanced TeX group as a flat token list including delimiters.
group :: Parser [Token]
group = (fmap (++) . (:)) <$> bgroup <*> tokens <*> count 1 egroup

-- Parse a control sequence and try to expand or execute it.
ctrlseq :: Parser [Token]
ctrlseq = do
  t@(CtrlSeq name active) <- ctrlseqNoExpand
  expandMode <- getExpandMode <$> getState
  if not expandMode
    then return [t]
    else do
      macros <- getMacros <$> getState
      case lookup (name, active) macros of
        Just m -> [] <$ expand ((name, active), m)
        Nothing -> execute t

---------------------------------------- Execute TeX primitives

-- Execute a primitive TeX control sequence.
execute :: Token -> Parser [Token]
execute (CtrlSeq name active) = expandMacro name active
execute t = error $ "Control sequence required. Cannot execute " ++ show t

expandMacro :: String -> Bool -> Parser [Token]
expandMacro name active = case (name, active) of
  ("begingroup", False) -> [CtrlSeq name active] <$ modifyState (pushGroup NativeGroup)
  ("endgroup", False) -> [CtrlSeq name active] <$ modifyState (popGroup NativeGroup)
  ("bgroup", False) -> [CtrlSeq name active] <$ modifyState (pushGroup AnonymousGroup)
  ("egroup", False) -> [CtrlSeq name active] <$ modifyState (popGroup AnonymousGroup)
  ("begin", False) -> beginEnvironment
  ("end", False) -> endEnvironment
  ("catcode", False) -> catcode
  ("def", False) -> def
  ("iftrue", False) -> iftrue
  ("iffalse", False) -> iffalse
  ("char", False) -> count 1 chr
  ("number", False) -> numbertoks
  ("NewDocumentCommand", False) -> declareDocumentCommand MacroNew
  ("RenewDocumentCommand", False) -> declareDocumentCommand MacroRenew
  ("ProvideDocumentCommand", False) -> declareDocumentCommand MacroProvide
  ("DeclareDocumentCommand", False) -> declareDocumentCommand MacroDeclare
  ("NewDocumentEnvironment", False) -> declareDocumentEnvironment MacroNew
  ("RenewDocumentEnvironment", False) -> declareDocumentEnvironment MacroRenew
  ("ProvideDocumentEnvironment", False) -> declareDocumentEnvironment MacroProvide
  ("DeclareDocumentEnvironment", False) -> declareDocumentEnvironment MacroDeclare
  ("IfBooleanTF", False) -> xparseif trueTok
  ("IfNoValueTF", False) -> xparseif noValueTok
  ("newcommand", False) -> newcommand MacroNew
  ("renewcommand", False) -> newcommand MacroRenew
  ("providecommand", False) -> newcommand MacroProvide
  ("DeclareRobustCommand", False) -> newcommand MacroDeclare
  ("newenvironment", False) -> newenvironment MacroNew
  ("renewenvironment", False) -> newenvironment MacroRenew
  _ -> return [CtrlSeq name active] -- undefined command

-------------------- Handle builtin macros

---------- Builtin macros: numbers

-- Parse a character by its number. We are treating these characters
-- as Tokens with catcode 'Other'.
chr :: Parser Token
chr = number >>= \chCode -> return (TeXChar (toEnum chCode) Other)

-- Convert an internal integer to its string representation.
numbertoks :: Parser [Token]
numbertoks = (map (`TeXChar` Other) . show) <$> number

---------- Builtin macros: conditionals

-- Expand a conditional to its left branch.
iftrue :: Parser [Token]
iftrue = [] <$ conditionalPush True

-- Expand a conditional to its right branch.
iffalse :: Parser [Token]
iffalse = [] <$ conditionalPush False

-- Expand a conditional and push the resulting token list
-- back into the stream (for group detection).
conditionalPush :: Bool -> Parser ()
conditionalPush = conditional >=> prependToInput

-- Parse a conditional and return its left or right branch,
-- depending on the provided flag.
--
-- The flag argument indicates whether the condition is true.
-- Note: Conditional nesting is independent of grouping,
-- so groups are not parsed here.
conditional :: Bool -> Parser [Token]
conditional b = do
  (leftToks, rightToks) <- withExpandMode b (condBranches b [])
  return $ if b then leftToks else rightToks

-- Parse the two branches of a conditional.
--
-- The flag argument indicates whether the condition is true.
condBranches :: Bool -> [Token] -> Parser ([Token], [Token])
condBranches b ltoks = do
  t <- tokenCond
  case t of
    [CtrlSeq name _]
      | name == "fi" ->
          return (ltoks, [])
      | name == "else" ->
          withExpandMode (not b) (condRightBranch []) >>=
            \rtoks -> return (ltoks, rtoks)
      | name `elem` ["iftrue", "iffalse"] ->
          -- handle embedded conditional in dead branch
          withoutExpansion (condBranches True []) *>
            condBranches b ltoks
      | otherwise ->
          condBranches b (ltoks ++ t)
    _ -> condBranches b (ltoks ++ t)

-- Parse the second branch of a conditional.
condRightBranch :: [Token] -> Parser [Token]
condRightBranch toks = do
  t <- tokenCond
  case t of
    [CtrlSeq name _]
      | name == "fi" ->
          return toks
      | name `elem` ["iftrue", "iffalse"] ->
        -- handle embedded conditional in dead branch
          withoutExpansion (condBranches True []) *>
            condRightBranch toks
      | otherwise ->
          condRightBranch (toks ++ t)
    _ -> condRightBranch (toks ++ t)

-- Parse a token in a conditional.
--
-- Note: Grouping characters are parsed literally.
tokenCond :: Parser [Token]
tokenCond = skipOptCommentsPAR *>
            (ctrlseq <|> count 1
             (charcc Bgroup <|> charcc Egroup <|>
              eolpar <|> param <|> someChar))

-- Evaluate an xparse-style conditional.
--
-- Note: In LaTeX3 this is defined via @\\ifx@.
xparseif :: Token -> Parser [Token]
xparseif t = do
  rs <- token <* skipOptSpace
  stripBraces <$> case stripBraces rs of
    [r] | r == t -> token <* skipOptSpace <* token
    _ -> token *> skipOptSpace *> token

---------- Builtin macros: catcodes

-- Parse the body of a @catcode@ command, execute it (by changing the
-- current catcode table) and remove catcode command from the token stream.
catcode :: Parser [Token]
catcode = do
  chCode <- number
  equals
  ccNew <- number -- Note: @toEnum@ will fail if not in range 0-15.
  modifyState (addCatcode (toEnum chCode, toEnum ccNew))
  return []

---------- Builtin macros: TeX macro definitions

-- Parse a macro definition, execute it (by updating the list of
-- active macros) and remove the @def@ command from the token stream.
def :: Parser [Token]
def = do
  -- preparation: disallow expansion of embedded macros
  expandMode <- getExpandMode <$> getState
  modifyState (setExpandMode False)
  -- parse the macro definition
  (CtrlSeq name active) <- ctrlseqNoExpand <?> "macro name"
  context <- macroContextDefinition <?> "macro context definition"
  body <- grouped tokens
  -- restore original expansion mode and register the new macro
  modifyState (setExpandMode expandMode)
  modifyState (registerLocalMacro
               ((name, active), (def2xparse context, body)))
  return []

-- Parse a macro context definition. Similar to 'tokens', but
-- must not contain 'Bgroup' (so do not include 'group' parser).
macroContextDefinition :: Parser [Token]
macroContextDefinition =
  concat <$> many (skipOptCommentsPAR *> count 1
                   (param <|> ctrlseqNoExpand <|> eolpar <|> someChar))

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
declareDocumentCommand :: MacroDefinitionMode -> Parser [Token]
declareDocumentCommand defMode = do
  -- preparation: disallow expansion of embedded macros
  expandMode <- getExpandMode <$> getState
  modifyState (setExpandMode False)
  -- parse the macro definition
  (CtrlSeq name active) <- optGrouped ctrlseqNoExpand <?> "macro name"
  context <- argspec <?> "macro argspec"
  body <- grouped tokens
  -- restore original expansion mode and register the new macro globally
  isDefined <- macroIsDefined (name, active) <$> getState
  modifyState (setExpandMode expandMode)
  modifyState $ macroDefinitionAction defMode isDefined
    ((name, active), (context, body))
  return []

-- Parse and register an xparse environment definition.
declareDocumentEnvironment :: MacroDefinitionMode -> Parser [Token]
declareDocumentEnvironment defMode = do
  -- preparation: disallow expansion of embedded macros
  expandMode <- getExpandMode <$> getState
  modifyState (setExpandMode False)
  -- parse the macro definition
  name <- grouped tokens <?> "environment name"
  context <- argspec <?> "environment argspec"
  startCode <- grouped tokens <?> "environment start code"
  endCode <- grouped tokens <?> "environment end code"
  -- restore original expansion mode and register the new environment globally
  isDefined <- macroEnvIsDefined name <$> getState
  modifyState (setExpandMode expandMode)
  modifyState $ macroEnvDefinitionAction defMode isDefined
    (name, MacroEnvDef context startCode endCode)
  return []

-- Parse a full xparse-style argument specification.
argspec :: Parser ArgSpec
argspec = grouped (skipOptSpace *> many argtype) <* skipOptSpace

-- Parse a single xparse-style argument type.
--
-- Not implemented: 'v' (verbatim), '>' (argument processor).
argtype :: Parser ArgType
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
            (TeXChar '[' Other) (TeXChar ']' Other)
            Nothing <$ letter 'o'
          , OptionalGroup <$>
            (letter 'd' *> singleToken) <*>
            singleToken <*> return Nothing
          , OptionalGroup
            (TeXChar '[' Other) (TeXChar ']' Other)
            <$> (letter 'O' *> (Just <$> grouped tokens))
          , OptionalGroup <$>
            (letter 'D' *> singleToken) <*>
            singleToken <*> (Just <$> grouped tokens)
          , OptionalGroupCC Bgroup Egroup Nothing <$
            letter 'g'
          , OptionalGroupCC Bgroup Egroup <$>
            (letter 'G' *> (Just <$> grouped tokens))
          , OptionalToken
            (TeXChar '*' Other) <$ letter 's'
          , OptionalToken
            <$> (letter 't' *> singleToken)
          ] <* skipOptSpace

---------- Builtin macros: LaTeX2e macro definitions

-- Parse and register a LaTeX2e macro definition.
newcommand :: MacroDefinitionMode -> Parser [Token]
newcommand defMode = do
  -- preparation: disallow expansion of embedded macros
  expandMode <- getExpandMode <$> getState
  modifyState (setExpandMode False)
  -- parse the macro definition
  optional (char '*' Other)
  (CtrlSeq name active) <- optGrouped ctrlseqNoExpand <?> "macro name"
  numArgs <- option 0 (decToInt <$> bracketed (count 1 digit))
  let open = TeXChar '[' Other
      close = TeXChar ']' Other
  optArg <- optionMaybe (balanced open close)
  let context = case optArg of
        Just d -> OptionalGroup open close (Just d) :
                  replicate (numArgs-1) Mandatory
        Nothing -> replicate numArgs Mandatory
  body <- grouped tokens <|> count 1 singleToken
  -- restore original expansion mode and register the new macro globally
  isDefined <- macroIsDefined (name, active) <$> getState
  modifyState (setExpandMode expandMode)
  modifyState $ macroDefinitionAction defMode isDefined
    ((name, active), (context, body))
  return []

-- Parse and register a LaTeX2e environment definition.
newenvironment :: MacroDefinitionMode -> Parser [Token]
newenvironment defMode = do
  -- preparation: disallow expansion of embedded macros
  expandMode <- getExpandMode <$> getState
  modifyState (setExpandMode False)
  -- parse the macro definition
  name <- grouped tokens <?> "environment name"
  numArgs <- option 0 (decToInt <$> bracketed (count 1 digit))
  let open = TeXChar '[' Other
      close = TeXChar ']' Other
  optArg <- optionMaybe (balanced open close)
  let context = case optArg of
        Just d -> OptionalGroup open close (Just d) :
                  replicate (numArgs-1) Mandatory
        Nothing -> replicate numArgs Mandatory
  startCode <- grouped tokens <?> "environment start code"
  endCode <- grouped tokens <?> "environment end code"
  -- restore original expansion mode and register the new macro globally
  isDefined <- macroEnvIsDefined name <$> getState
  modifyState (setExpandMode expandMode)
  modifyState $ macroEnvDefinitionAction defMode isDefined
    (name, MacroEnvDef context startCode endCode)
  return []

-------------------- Handle LaTeX environments (named groups)

-- Start TeX group and try to expand user-defined environment definitions.
beginEnvironment :: Parser [Token]
beginEnvironment = do
  name <- envName
  -- Note: expansion must be enabled because we are expanding 'begin'
  macroEnvs <- getMacroEnvs <$> getState
  case lookup (stripBraces name) macroEnvs of
    Nothing -> let grp = NamedGroup (stripBraces name)
               in modifyState (pushGroup grp) *>
                  return (CtrlSeq "begin" False: name)
    Just envdef -> do
      (startCode, endCode) <- expandEnvironment envdef
      modifyState . pushGroup $
        DefinedGroup (stripBraces name) startCode endCode
      prependToInput startCode
      return []

-- Close matching TeX group and inject end code for user-defined environments.
endEnvironment :: Parser [Token]
endEnvironment = do
  name <- envName
  let endEnv = CtrlSeq "end" False: name
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
        prependToInput (endCode ++ endEnv)
        return []
    _ -> do
      modifyState (popGroup (NamedGroup (stripBraces name)))
      return endEnv

-- Parse the name of a LaTeX environment, including group delimiters.
--
-- We currently allow arbitrary token lists.
envName :: Parser [Token]
envName = group

-------------------- Parser state utility functions

-- Run a parser with expansion enabled or disabled,
-- depending on the provided flag.
withExpandMode :: Bool -> Parser a -> Parser a
withExpandMode mode parser = do
  expandMode <- getExpandMode <$> getState
  modifyState (setExpandMode mode)
  result <- parser
  modifyState (setExpandMode expandMode)
  return result

-- Run a parser with expansion disabled.
withoutExpansion :: Parser a -> Parser a
withoutExpansion = withExpandMode False
