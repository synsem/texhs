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
  macros <- getMacroCmds <$> getState
  case lookup (name, active) macros of
    Just m -> [] <$ expand m
    Nothing -> execute t

---------------------------------------- Execute TeX primitives

-- | Primitives are internal names of executable commands
-- and possible meanings of control sequences.
type Primitive = String

-- | Default mapping of tokens (control sequences) to primitives.
defaultPrimitives :: [(Token, Primitive)]
defaultPrimitives = map wrapCtrlSeq
  [ "begingroup"
  , "endgroup"
  , "bgroup"
  , "egroup"
  , "begin"
  , "end"
  , "catcode"
  , "def"
  , "iftrue"
  , "iffalse"
  , "char"
  , "number"
  , "NewDocumentCommand"
  , "RenewDocumentCommand"
  , "ProvideDocumentCommand"
  , "DeclareDocumentCommand"
  , "NewDocumentEnvironment"
  , "RenewDocumentEnvironment"
  , "ProvideDocumentEnvironment"
  , "DeclareDocumentEnvironment"
  , "IfBooleanTF"
  , "IfNoValueTF"
  , "newcommand"
  , "renewcommand"
  , "providecommand"
  , "DeclareRobustCommand"
  , "newenvironment"
  , "renewenvironment"
  ]
  where wrapCtrlSeq t = (CtrlSeq t False, t)

-- | Meanings of primitives.
primitiveMeanings :: [(Primitive, Parser [Token])]
primitiveMeanings =
  [ ("begingroup", [CtrlSeq "begingroup" False] <$ modifyState (pushGroup NativeGroup))
  , ("endgroup", [CtrlSeq "endgroup" False] <$ modifyState (popGroup NativeGroup))
  , ("bgroup", [CtrlSeq "bgroup" False] <$ modifyState (pushGroup AnonymousGroup))
  , ("egroup", [CtrlSeq "egroup" False] <$ modifyState (popGroup AnonymousGroup))
  , ("begin", beginEnvironment)
  , ("end", endEnvironment)
  , ("catcode", catcode)
  , ("def", def)
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
  ]

-- | Execute a token if its meaning is a primitive,
-- else return the token unmodified.
execute :: Token -> Parser [Token]
execute t = case lookup t defaultPrimitives of
  Just p -> executePrimitive p
  Nothing -> return [t]

-- | Execute a primitive command.
executePrimitive :: Primitive -> Parser [Token]
executePrimitive name = case lookup name primitiveMeanings of
  Just p -> p
  Nothing -> error $ "Call to undefined primitive: " ++ name

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
  let (l,r) = (b, not b) -- branch expansion modes
  (leftToks, rightToks) <- condBranches (l,r) []
  return $ if b then leftToks else rightToks

-- Parse the two branches of a conditional.
--
-- The flag arguments indicate whether to expand the branches.
condBranches :: (Bool, Bool) -> [Token] -> Parser ([Token], [Token])
condBranches (expandLeft,expandRight) ltoks = do
  t <- tokenCond expandLeft
  case t of
    [CtrlSeq name _]
      | name == "fi" ->
          return (ltoks, [])
      | name == "else" ->
          (,) ltoks <$> condRightBranch expandRight []
      | name `elem` ["iftrue", "iffalse"] ->
          -- handle embedded conditional in dead branch
          condBranches (False, False) [] *>
            condBranches (expandLeft, expandRight) ltoks
      | otherwise ->
          condBranches (expandLeft, expandRight) (ltoks ++ t)
    _ -> condBranches (expandLeft, expandRight) (ltoks ++ t)

-- Parse the second branch of a conditional.
--
-- The flag argument indicates whether to expand the branch.
condRightBranch :: Bool -> [Token] -> Parser [Token]
condRightBranch expandMode toks = do
  t <- tokenCond expandMode
  case t of
    [CtrlSeq name _]
      | name == "fi" ->
          return toks
      | name `elem` ["iftrue", "iffalse"] ->
        -- handle embedded conditional in dead branch
          condBranches (False, False) [] *>
            condRightBranch expandMode toks
      | otherwise ->
          condRightBranch expandMode (toks ++ t)
    _ -> condRightBranch expandMode (toks ++ t)

-- Parse a token in a conditional.
--
-- Note: Grouping characters are parsed literally.
tokenCond :: Bool -> Parser [Token]
tokenCond expandMode = skipOptCommentsPAR *>
            ((if expandMode then ctrlseq else count 1 ctrlseqNoExpand) <|>
             count 1 (charcc Bgroup <|> charcc Egroup <|>
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
  (CtrlSeq name active) <- ctrlseqNoExpand <?> "macro name"
  context <- macroContextDefinition <?> "macro context definition"
  body <- grouped tokensNoExpand
  modifyState (registerLocalMacroCmd
               ((name, active), MacroCmdDef (def2xparse context) body))
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
  (CtrlSeq name active) <- optGrouped ctrlseqNoExpand <?> "macro name"
  context <- argspec <?> "macro argspec"
  body <- grouped tokensNoExpand
  isDefined <- macroCmdIsDefined (name, active) <$> getState
  modifyState $ macroCmdDefinitionAction defMode isDefined
    ((name, active), MacroCmdDef context body)
  return []

-- Parse and register an xparse environment definition.
declareDocumentEnvironment :: MacroDefinitionMode -> Parser [Token]
declareDocumentEnvironment defMode = do
  name <- grouped tokensNoExpand <?> "environment name"
  context <- argspec <?> "environment argspec"
  startCode <- grouped tokensNoExpand <?> "environment start code"
  endCode <- grouped tokensNoExpand <?> "environment end code"
  isDefined <- macroEnvIsDefined name <$> getState
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
  optional (char '*' Other) -- ignore 'long' property
  (CtrlSeq name active) <- optGrouped ctrlseqNoExpand <?> "macro name"
  numArgs <- option 0 (bracketed singleDigit)
  let open = TeXChar '[' Other
      close = TeXChar ']' Other
  optArg <- optionMaybe (balanced open close)
  let context = case optArg of
        Just d -> OptionalGroup open close (Just d) :
                  replicate (numArgs-1) Mandatory
        Nothing -> replicate numArgs Mandatory
  body <- grouped tokensNoExpand <|> count 1 singleToken
  isDefined <- macroCmdIsDefined (name, active) <$> getState
  modifyState $ macroCmdDefinitionAction defMode isDefined
    ((name, active), MacroCmdDef context body)
  return []

-- Parse and register a LaTeX2e environment definition.
newenvironment :: MacroDefinitionMode -> Parser [Token]
newenvironment defMode = do
  name <- grouped tokens <?> "environment name"
  numArgs <- option 0 (bracketed singleDigit)
  let open = TeXChar '[' Other
      close = TeXChar ']' Other
  optArg <- optionMaybe (balanced open close)
  let context = case optArg of
        Just d -> OptionalGroup open close (Just d) :
                  replicate (numArgs-1) Mandatory
        Nothing -> replicate numArgs Mandatory
  startCode <- grouped tokensNoExpand <?> "environment start code"
  endCode <- grouped tokensNoExpand <?> "environment end code"
  isDefined <- macroEnvIsDefined name <$> getState
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
