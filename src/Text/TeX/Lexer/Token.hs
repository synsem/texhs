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
  , CharOrToken
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
  ) where

import Text.TeX.Lexer.Catcode (Catcode(..))

-------------------- Token types

-- The input to the lexer/parser is a stream of @CharOrToken@
-- elements. These are either (1) unparsed and yet unseen raw @Char@
-- elements (i.e. characters that need to be assigned a catcode or to
-- be assembled into control sequences) or (2) already parsed @Token@
-- elements (e.g. characters with existing catcode assignment, like
-- they are stored in macros). All @Token@ elements in this stream are
-- the result of macro expansion, and they always form a prefix of the
-- stream (i.e. they never appear after a raw @Char@ element).

-- NOTE: We are (ab)using @Either@ to hold two equally relevant types.
-- In particular, @Left@ values are not to be interpreted as errors.
-- | This is the type of elements of a TeX input stream.
type CharOrToken = Either Char Token

-- | A TeX Token is a character, a control sequence, or a parameter.
data Token = TeXChar { getRawChar :: Char, getCatcode :: Catcode }
           | CtrlSeq { getName :: String, isActive :: Bool }
           | Param { getParamid :: Int, getNesting :: Int }
           deriving Eq

instance Show Token where
  show (TeXChar ch cc) = '\'':ch:"\'" ++ "(" ++ show cc ++ ")"
  show (CtrlSeq cs active) = "<" ++ cs ++ (if active then "(active)" else "") ++ ">"
  show (Param i n) = replicate n '#' ++ show i

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
