----------------------------------------------------------------------
-- |
-- Module      :  Text.TeX.Parser.Types
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Types and utility functions for TeX documents.
----------------------------------------------------------------------

module Text.TeX.Parser.Types
  ( -- * TeX types
    TeX
  , TeXAtom(..)
  , Args
  , MathType(..)
    -- * TeXAtom predicates
  , isPlain
  , isCommand
  , isGroup
  , isMathGroup
  , isSupScript
  , isSubScript
  , isAlignMark
  , isWhite
  , isNewline
  , isPar
    -- * Utility functions
  , normalize
  ) where

-------------------- TeX types

-- | A TeX document consist of a list of TeX atoms.
type TeX = [TeXAtom]

-- | A pair of optional and mandatory arguments
-- of a TeX command or environment.
type Args = ([TeX], [TeX])

-- | Type of a 'MathGroup'.
data MathType = MathInline | MathDisplay
  deriving (Show, Eq)

-- | Atomic elements of a 'TeX' document.
data TeXAtom
  = Plain String
    -- ^ a character or a word
  | Command String Args
    -- ^ a macro command with its name and arguments
  | Group String Args TeX
    -- ^ a group with its name, arguments and body
  | MathGroup MathType TeX
    -- ^ a math group with its type and body
  | SupScript TeX
    -- ^ superscripted content
  | SubScript TeX
    -- ^ subscripted content
  | AlignMark
    -- ^ an align tab, @&@ in Plain TeX and LaTeX
  | White
    -- ^ whitespace (spaces, tabs, newlines) within a paragraph
  | Newline
    -- ^ a hard newline, @\\\\@ in Plain TeX and LaTeX
  | Par
    -- ^ paragraph break
  deriving (Eq, Show)

-------------------- TeXAtom predicates

-- | Test whether a 'TeXAtom' is a 'Plain' text element.
isPlain :: TeXAtom -> Bool
isPlain (Plain _) = True
isPlain _ = False

-- | Test whether a 'TeXAtom' is a 'Command'.
isCommand :: TeXAtom -> Bool
isCommand (Command _ _) = True
isCommand _ = False

-- | Test whether a 'TeXAtom' is a 'Group'.
isGroup :: TeXAtom -> Bool
isGroup (Group _ _ _) = True
isGroup _ = False

-- | Test whether a 'TeXAtom' is a 'MathGroup'.
isMathGroup :: TeXAtom -> Bool
isMathGroup (MathGroup _ _) = True
isMathGroup _ = False

-- | Test whether a 'TeXAtom' is a 'SupScript'.
isSupScript :: TeXAtom -> Bool
isSupScript (SupScript _) = True
isSupScript _ = False

-- | Test whether a 'TeXAtom' is a 'SubScript'.
isSubScript :: TeXAtom -> Bool
isSubScript (SubScript _) = True
isSubScript _ = False

-- | Test whether a 'TeXAtom' is an 'AlignMark'.
isAlignMark :: TeXAtom -> Bool
isAlignMark AlignMark = True
isAlignMark _ = False

-- | Test whether a 'TeXAtom' is a 'White'.
isWhite :: TeXAtom -> Bool
isWhite White = True
isWhite _ = False

-- | Test whether a 'TeXAtom' is a 'Newline'.
isNewline :: TeXAtom -> Bool
isNewline Newline = True
isNewline _ = False

-- | Test whether a 'TeXAtom' is a 'Par'.
isPar :: TeXAtom -> Bool
isPar Par = True
isPar _ = False

-------------------- Utility functions

-- | Conflate intra-level adjacent whitespace.
--
-- This will not remove redundant whitespace completely. In
-- particular, it will not strip leading or trailing whitespace and it
-- will not collapse inter-level adjacent whitespace. For example,
-- both spaces in \"@a { }b@\" and in \"@a{ }{ }b@\" will be kept.
normalize :: TeX -> TeX
normalize [] = []
normalize (White:xs) = case dropWhile isWhite xs of
  (Newline:ys) -> Newline : normalize (dropWhile isWhite ys)
  ys@(Par:_) -> normalize ys
  ys -> White : normalize ys
normalize (Par:xs) = Par : normalize (dropWhile (\x -> isWhite x || isPar x) xs)
normalize (x:xs) = fmapAtom normalize x : normalize xs

-- Lift a 'TeX' function to an 'Args' function.
fmapArgs :: (TeX -> TeX) -> Args -> Args
fmapArgs f (oargs, margs) = (map f oargs, map f margs)

-- Apply a 'TeX' function to a 'TeXAtom'.
fmapAtom :: (TeX -> TeX) -> TeXAtom -> TeXAtom
fmapAtom _ (Plain content) = Plain content
fmapAtom f (Command name args) = Command name (fmapArgs f args)
fmapAtom f (Group name args body) = Group name (fmapArgs f args) (f body)
fmapAtom f (MathGroup mtype body) = MathGroup mtype (f body)
fmapAtom f (SupScript body) = SupScript (f body)
fmapAtom f (SubScript body) = SubScript (f body)
fmapAtom _ AlignMark = AlignMark
fmapAtom _ White = White
fmapAtom _ Newline = Newline
fmapAtom _ Par = Par
