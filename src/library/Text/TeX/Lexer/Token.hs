----------------------------------------------------------------------
-- |
-- Module      :  Text.TeX.Lexer.Token
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  portable
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
    -- * Token constants
    -- ** Number prefixes
  , hexPrefix
  , octPrefix
  , ordPrefix
    -- ** Primitive control sequences
  , parTok
  , falseTok
  , trueTok
  , noValueTok
  ) where

import Text.TeX.Lexer.Catcode (Catcode(..))


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
-- (Token @t@ is a character satisfying the property @p@.)
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

-------------------- TeX constants: Number prefixes

-- | Prefix for hex constants.
hexPrefix :: Token
hexPrefix = TeXChar '"' Other

-- | Prefix for octal constants.
octPrefix :: Token
octPrefix = TeXChar '\'' Other

-- | Prefix for alpha constants.
ordPrefix :: Token
ordPrefix = TeXChar '`' Other

-------------------- TeX constants: Primitive control sequences

-- | TeX paragraph break.
parTok :: Token
parTok = CtrlSeq "par" False

-- | Boolean constant used by xparse.
--
-- Note: We are treating @\\BooleanFalse@ as a primitive here,
-- although it is defined as @\\c_false_bool@ in xparse, which in turn
-- is defined as @\\char 0@ in l3basics.
falseTok :: Token
falseTok = CtrlSeq "BooleanFalse" False

-- | Boolean constant used by xparse.
--
-- Note: We are treating @\\BooleanTrue@ as a primitive here,
-- although it is defined as @\\c_true_bool@ in xparse, which in turn
-- is defined as @\\char 1@ in l3basics.
trueTok :: Token
trueTok = CtrlSeq "BooleanTrue" False

-- | Used by xparse to represent a missing argument,
-- printed as @-NoValue-@.
noValueTok :: Token
noValueTok = CtrlSeq "NoValue" False
