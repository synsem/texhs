----------------------------------------------------------------------
-- |
-- Module      :  Text.TeX.Lexer.Token
-- Copyright   :  2015-2016 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  GHC
--
-- Types and utility functions for TeX tokens.
----------------------------------------------------------------------

module Text.TeX.Lexer.Token
  ( -- * TeX token types
    Token(..)
  , Group(..)
    -- * Utility functions
    -- ** Token predicates
  , isTeXChar
  , isCtrlSeq
  , isParam
    -- ** TeXChar predicates
  , isCharEq
  , isCharSat
  , hasCC
    -- ** Token list manipulation
  , stripBraces
    -- * Token deconstruction
  , detokenize
    -- * Token builder functions
  , mkDefault
  , mkQuote
  , mkLetter
  , mkString
  , mkOther
  , mkCtrlSeq
  , mkGroup
  , mkOptArg
  , mkEnv
    -- * Token constants
    -- ** Number prefixes
  , hexPrefix
  , octPrefix
  , ordPrefix
    -- ** Control sequences
  , parTok
  , falseTok
  , trueTok
  , noValueTok
    -- ** Basic tokens for various catcodes
  , bgroupTok
  , egroupTok
  , mathTok
  , alignTok
  , eolTok
  , supTok
  , subTok
  , spcTok
  ) where

import Text.TeX.Lexer.Catcode


-------------------- Token types

-- | A TeX Token is a character, a control sequence, or a parameter.
data Token = TeXChar { getRawChar :: Char, getCatcode :: Catcode }
           | CtrlSeq { getName :: String, isActive :: Bool }
           | Param { getParamid :: Int, getNesting :: Int }
           deriving Eq

instance Show Token where
  show (TeXChar ch cc) = '\'':ch:"\'" ++ "(" ++ show cc ++ ")"
  show (CtrlSeq cs active) = "<" ++ cs ++ (if active then "(active)" else "") ++ ">"
  show (Param i n) = replicate n '#' ++ show i

-- | TeX groups and LaTeX environments.
data Group = AnonymousGroup
             -- ^ Standard TeX group: @{ .. }@ or @\\bgroup .. \\egroup@.
           | NativeGroup
             -- ^ Balanced TeX group: @\\begingroup .. \\endgroup@.
           | NamedGroup [Token]
             -- ^ LaTeX Environment: @\\begin{name} .. \\end{name}@.
           | DefinedGroup [Token] [Token] [Token]
             -- ^ LaTeX Environment with known (user defined) expansion.
             -- Arguments: @name@, @before@ (start code), @after@ (end code).
           deriving Show

instance Eq Group where
  AnonymousGroup == AnonymousGroup = True
  NativeGroup == NativeGroup = True
  NamedGroup n == NamedGroup m = n == m
  DefinedGroup n _ _ == DefinedGroup m _ _ = n == m
  _ == _ = False

-------------------- Token predicates

-- | Test whether a 'Token' is a 'TeXChar'.
isTeXChar :: Token -> Bool
isTeXChar (TeXChar _ _) = True
isTeXChar _ = False

-- | Test whether a 'Token' is a 'CtrlSeq'.
isCtrlSeq :: Token -> Bool
isCtrlSeq (CtrlSeq _ _) = True
isCtrlSeq _ = False

-- | Test whether a 'Token' is a 'Param'.
isParam :: Token -> Bool
isParam (Param _ _) = True
isParam _ = False

-------------------- TeXChar predicates

-- | Equality for 'TeXChar'.
isCharEq :: Char -> Catcode -> Token -> Bool
isCharEq c1 i1 (TeXChar c2 i2) = c1 == c2 && i1 == i2
isCharEq _ _ _ = False

-- | Lift a @Char@ property to a 'Token' property.
--
-- Given a character property and a 'Token', return @True@
-- iff the token is a 'TeXChar' with the given property.
isCharSat :: (Char -> Bool) -> Token -> Bool
isCharSat p (TeXChar c _) = p c
isCharSat _ _ = False

-- | Test whether a token is a 'TeXChar' with the provided catcode.
hasCC :: Catcode -> Token -> Bool
hasCC cc (TeXChar _ i) = i == cc
hasCC _ _ = False

-------------------- Token list manipulation

-- | If the token list is wrapped in matching braces, drop them.
-- (This can be used to strip braces from arguments in macro calls.)
stripBraces :: [Token] -> [Token]
stripBraces [] = []
stripBraces toks@(x:xs) = if hasCC Bgroup x && hasCC Egroup (last toks)
                          then init xs
                          else toks

-------------------- Token deconstruction

-- | Deconstruct a token to its string representation.
detokenize :: Token -> String
detokenize (TeXChar ch _) = [ch]
detokenize (CtrlSeq name _) = name
detokenize p@(Param _ _) = show p

-------------------- Token builder functions

-- | Assign default catcodes to a list of characters.
mkDefault :: String -> [Token]
mkDefault = map (\c -> TeXChar c (defaultCatcodeOf c))

-- | Assign quote catcodes to a list of characters.
-- See 'quoteCatcodeOf'.
mkQuote :: String -> [Token]
mkQuote = map (\c -> TeXChar c (quoteCatcodeOf c))

-- | Create a 'Letter' token.
mkLetter :: Char -> Token
mkLetter = flip TeXChar Letter

-- | Create a list of 'Letter' tokens.
mkString :: String -> [Token]
mkString = map mkLetter

-- | Create an 'Other' token.
mkOther :: Char -> Token
mkOther = flip TeXChar Other

-- | Create a 'CtrlSeq' token.
mkCtrlSeq :: String -> Token
mkCtrlSeq = flip CtrlSeq False

-- | Wrap a list of tokens in a TeX group.
mkGroup :: [Token] -> [Token]
mkGroup = between bgroupTok egroupTok

-- | Wrap a list of tokens in square brackets.
mkOptArg :: [Token] -> [Token]
mkOptArg = between (mkOther '[') (mkOther ']')

-- | Create a LaTeX-style environment (no arguments):
-- @\\begin{name} body \\end{name}@.
mkEnv :: String -> [Token] -> [Token]
mkEnv name body =
  mkCtrlSeq "begin" : mkGroup (mkString name) ++ body ++
  (mkCtrlSeq "end" : mkGroup (mkString name))

between :: Token -> Token -> [Token] -> [Token]
between open close content = open : (content ++ [close])

-------------------- TeX constants: Number prefixes

-- | Prefix for hex constants.
hexPrefix :: Token
hexPrefix = mkOther '"'

-- | Prefix for octal constants.
octPrefix :: Token
octPrefix = mkOther '\''

-- | Prefix for alpha constants.
ordPrefix :: Token
ordPrefix = mkOther '`'

-------------------- TeX constants: Control sequences

-- | TeX paragraph break.
--
-- Note: This token is used by the lexer to represent
-- any inter-paragraph whitespace.
parTok :: Token
parTok = mkCtrlSeq "par"

-- | Boolean constant used by xparse.
--
-- Note: We are treating @\\BooleanFalse@ as a primitive here,
-- although it is defined as @\\c_false_bool@ in xparse, which in turn
-- is defined as @\\char 0@ in l3basics.
falseTok :: Token
falseTok = mkCtrlSeq "BooleanFalse"

-- | Boolean constant used by xparse.
--
-- Note: We are treating @\\BooleanTrue@ as a primitive here,
-- although it is defined as @\\c_true_bool@ in xparse, which in turn
-- is defined as @\\char 1@ in l3basics.
trueTok :: Token
trueTok = mkCtrlSeq "BooleanTrue"

-- | Used by xparse to represent a missing argument,
-- printed as @-NoValue-@.
noValueTok :: Token
noValueTok = mkCtrlSeq "NoValue"

-------------------- TeX constants: Basic tokens for various catcodes

-- | 'Bgroup' token.
bgroupTok :: Token
bgroupTok = TeXChar '{' Bgroup

-- | 'Egroup' token.
egroupTok :: Token
egroupTok = TeXChar '}' Egroup

-- | 'Mathshift' token.
mathTok :: Token
mathTok = TeXChar '$' Mathshift

-- | 'AlignTab' token.
alignTok :: Token
alignTok = TeXChar '&' AlignTab

-- | 'Eol' token.
--
-- Note: This token does not appear in the lexer output.
-- All intra-paragraph whitespace is represented by 'spcTok', and
-- all inter-paragraph whitespace is represented by 'parTok'.
eolTok :: Token
eolTok = TeXChar '\n' Eol

-- | 'Supscript' token.
supTok :: Token
supTok = TeXChar '^' Supscript

-- | 'Subscript' token.
subTok :: Token
subTok = TeXChar '_' Subscript

-- | 'Space' token.
--
-- Note: This token is used by the lexer to represent
-- any intra-paragraph whitespace.
spcTok :: Token
spcTok = TeXChar ' ' Space
