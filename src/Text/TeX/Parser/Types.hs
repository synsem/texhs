----------------------------------------------------------------------
-- |
-- Module      :  Text.TeX.Parser.Types
-- Copyright   :  2015-2016 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  GHC
--
-- Types and utility functions for TeX documents.
----------------------------------------------------------------------

module Text.TeX.Parser.Types
  ( -- * TeX types
    TeX
  , TeXAtom(..)
  , Arg(..)
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
  , isCmd
  , isGrp
    -- * Access arguments
  , getOptArg
  , getOblArg
  ) where

-------------------- TeX types

-- | A TeX document consist of a list of TeX atoms.
type TeX = [TeXAtom]

-- | An obligatory or optional argument of a TeX command.
data Arg = OblArg TeX | OptArg TeX
  deriving (Show, Eq)

-- | Arguments of a TeX command.
type Args = [Arg]

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


-- | Test whether a 'TeXAtom' is a specific command.
isCmd :: String -> TeXAtom -> Bool
isCmd n (Command name _) = n == name
isCmd _ _ = False

-- | Test whether a 'TeXAtom' is a specific group.
isGrp :: String -> TeXAtom -> Bool
isGrp n (Group name _ _) = n == name
isGrp _ _ = False


-- | Test whether an 'Arg' is obligatory.
isOblArg :: Arg -> Bool
isOblArg (OblArg _) = True
isOblArg (OptArg _) = False

-- | Test whether an 'Arg' is optional.
isOptArg :: Arg -> Bool
isOptArg = not . isOblArg


-------------------- Access arguments

-- | Retrieve the n-th optional argument (of a command or environment).
getOptArg :: Int -> Args -> TeX
getOptArg n args = getArg isOptArg n args

-- | Retrieve the n-th obligatory argument (of a command or environment).
getOblArg :: Int -> Args -> TeX
getOblArg n args = getArg isOblArg n args

-- Retrieve the n-th argument (of a command or environment).
-- Expects an 'Arg' parameter for filtering an argument list.
-- Used as helper for 'getOptArg' and 'getOblArg'.
getArg :: (Arg -> Bool) -> Int -> Args -> TeX
getArg p n args =
  let subArgs = filter p args
  in if n >= 0 && length subArgs > n
     then unArg (subArgs !! n)
     else []

-- Extract content of an argument.
unArg :: Arg -> TeX
unArg (OblArg xs) = xs
unArg (OptArg xs) = xs
