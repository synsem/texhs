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
  , isStarred
    -- * Argument specification
  , ArgSpec
  , ArgType(..)
  , ArgSpecSimple
  , toArgSpec
  ) where


-------------------- TeX types

-- | A TeX document consist of a list of TeX atoms.
type TeX = [TeXAtom]

-- | An obligatory or optional argument of a TeX command.
data Arg
  = OblArg TeX
  | OptArg TeX
  | StarArg
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
    -- ^ a command with its name and arguments
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
isPlain Plain{} = True
isPlain _ = False

-- | Test whether a 'TeXAtom' is a 'Command'.
isCommand :: TeXAtom -> Bool
isCommand Command{} = True
isCommand _ = False

-- | Test whether a 'TeXAtom' is a 'Group'.
isGroup :: TeXAtom -> Bool
isGroup Group{} = True
isGroup _ = False

-- | Test whether a 'TeXAtom' is a 'MathGroup'.
isMathGroup :: TeXAtom -> Bool
isMathGroup MathGroup{} = True
isMathGroup _ = False

-- | Test whether a 'TeXAtom' is a 'SupScript'.
isSupScript :: TeXAtom -> Bool
isSupScript SupScript{} = True
isSupScript _ = False

-- | Test whether a 'TeXAtom' is a 'SubScript'.
isSubScript :: TeXAtom -> Bool
isSubScript SubScript{} = True
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
isOblArg OblArg{} = True
isOblArg _ = False

-- | Test whether an 'Arg' is optional.
isOptArg :: Arg -> Bool
isOptArg OptArg{} = True
isOptArg _ = False

-- | Test whether an 'Arg' is a star token.
isStarArg :: Arg -> Bool
isStarArg StarArg = True
isStarArg _ = False


-------------------- Access arguments

-- | Test whether a 'Command' is starred
-- by looking for a 'StarArg' in its 'Args'.
isStarred :: Args -> Bool
isStarred = any isStarArg

-- | Retrieve the n-th optional argument (of a command or environment).
getOptArg :: Int -> Args -> TeX
getOptArg = getArg isOptArg

-- | Retrieve the n-th obligatory argument (of a command or environment).
getOblArg :: Int -> Args -> TeX
getOblArg = getArg isOblArg

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
unArg StarArg = []

-------------------- Argument Specification for (non-user) commands

-- | A list of argument types that a command expects.
type ArgSpec = [ArgType]

-- Note: For user-defined macros, see "Text.TeX.Lexer.Macro" instead.
-- | Possible argument types for (non-user) commands.
data ArgType
     -- | Mandatory argument (group or single token).
  = Mandatory
    -- | Optional argument in brackets. This shortcut is
    --   equivalent to @OptionalGroup \'[\' \']\'@.
  | Optional
    -- | Optional argument between custom delimiters. The delimiting
    --   characters will only be matched against character tokens
    --   that have the category code Letter or Other.
  | OptionalGroup Char Char
    -- | Optional star token (for \"starred\" LaTeX commands).
  | OptionalStar
  deriving (Eq, Show)

-- | A simplified representation of the 'ArgSpec' for a command
-- as a pair consisting of the number of optional and the number
-- of mandatory arguments that the command takes.
type ArgSpecSimple = (Int, Int)

-- | Convert from 'ArgSpecSimple' to a proper 'ArgSpec'.
toArgSpec :: ArgSpecSimple -> ArgSpec
toArgSpec (opt, obl) = replicate opt Optional ++ replicate obl Mandatory
