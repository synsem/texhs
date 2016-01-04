----------------------------------------------------------------------
-- |
-- Module      :  Text.TeX.Lexer.Catcode
-- Copyright   :  2015-2016 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  GHC
--
-- Types and utility functions for TeX catcodes and catcode tables.
----------------------------------------------------------------------

module Text.TeX.Lexer.Catcode
  ( -- * Catcodes
    Catcode(..)
  , showCatcodePP
    -- * Catcode groups
  , catcodesAllowed
  , catcodesNonescaped
  , catcodesPassive
    -- * Catcode tables
  , CatcodeTable
  , defaultCatcodeTable
    -- ** Catcode table lookup (read)
  , catcodeOf
  , defaultCatcodeOf
  , quoteCatcodeOf
  , hasCatcode
    -- ** Catcode table modifications (write)
  , updateCatcodeTable
  ) where

import Data.Char (isAlpha, isSpace)
import Data.Maybe (fromMaybe)


-- | TeX catcodes. The @Enum@ instance respects the canonical mapping
-- of catcodes to the integers 0-15.
data Catcode
  = Escape
  | Bgroup
  | Egroup
  | Mathshift
  | AlignTab
  | Eol
  | ParamPrefix
  | Supscript
  | Subscript
  | Ignored
  | Space
  | Letter
  | Other
  | Active
  | Comment
  | Invalid
  deriving (Eq, Ord, Enum)

instance Show Catcode where
  show = show . fromEnum

-- | Show the name of a category code,
-- mostly as reported by TeX's @\\meaning@.
-- See TeX82, §298.
showCatcodePP :: Catcode -> String
showCatcodePP Escape = "escape character"
showCatcodePP Bgroup = "begin-group character"
showCatcodePP Egroup = "end-group character"
showCatcodePP Mathshift = "math shift character"
showCatcodePP AlignTab = "alignment tab character"
showCatcodePP Eol = "end of line character"
showCatcodePP ParamPrefix = "macro parameter character"
showCatcodePP Supscript = "superscript character"
showCatcodePP Subscript = "subscript character"
showCatcodePP Ignored = "ignored character"
showCatcodePP Space = "space character"
showCatcodePP Letter = "the letter"
showCatcodePP Other = "the character"
showCatcodePP Active = "active character"
showCatcodePP Comment = "comment character"
showCatcodePP Invalid = "invalid character"

-- | A catcode table is a (typically partial) assignment of catcodes to
-- characters. If a character has no entry in this table, its catcode
-- is determined as follows: If it is a Unicode letter, it has catcode
-- 'Letter'; otherwise it has catcode 'Other'. See 'catcodeOf'.
type CatcodeTable = [(Char, Catcode)]

-- | The default catcode table setup by plain TeX and LaTeX.
defaultCatcodeTable :: CatcodeTable
defaultCatcodeTable =
  [ ('\\', Escape)
  , ('{', Bgroup)
  , ('}', Egroup)
  , ('$', Mathshift)
  , ('&', AlignTab)
  , ('\n', Eol)
  , ('#', ParamPrefix)
  , ('^', Supscript)
  , ('_', Subscript)
  , ('\0', Ignored)
  , (' ', Space)
  , ('\t', Space)
  , ('~', Active)
  , ('%', Comment)
  , ('\DEL', Invalid)
  ]

---------- Catcode groups

-- | /Allowed/ catcodes are all catcodes except those ignored by TeX,
-- namely 'Ignored' and 'Invalid'.
catcodesAllowed :: [Catcode]
catcodesAllowed = Escape : catcodesNonescaped

-- | /Non-escaped/ catcodes are all /allowed/ catcodes except 'Escape'.
catcodesNonescaped :: [Catcode]
catcodesNonescaped =
  catcodesPassive ++
  [Bgroup, Egroup,
   Eol, ParamPrefix, Space,
   Active, Comment]

-- | /Passive/ catcodes are catcodes that are treated as literal characters
-- by the lexer. These are all catcodes except:
--
-- (1) those that have dedicated parsers in "Text.TeX.Lexer"
--     ('Escape', 'Bgroup', 'Egroup', 'Eol', 'ParamPrefix',
--     'Space', 'Active', 'Comment'), and
--
-- (2) those that are ignored by TeX
--     ('Ignored', 'Invalid').
catcodesPassive :: [Catcode]
catcodesPassive =
  [Letter, Other,
   Mathshift, AlignTab,
   Supscript, Subscript]

---------- Catcode table lookup (read)

-- | Lookup the catcode for the character @ch@ relative to catcode
-- table @t@. If @ch@ is not listed in @t@, Unicode letters default to
-- catcode 'Letter', all other characters default to catcode 'Other'.
catcodeOf :: Char -> CatcodeTable -> Catcode
catcodeOf ch t = fromMaybe
                 (if isAlpha ch then Letter else Other)
                 (lookup ch t)

-- | Lookup the default catcode for a character,
-- using 'defaultCatcodeTable'.
defaultCatcodeOf :: Char -> Catcode
defaultCatcodeOf = flip catcodeOf defaultCatcodeTable

-- | Lookup the default quoting catcode for a character:
-- letters are treated are 'Letter' tokens,
-- whitespace characters as 'Space' tokens (no 'Eol') and
-- all other characters as 'Other' tokens.
quoteCatcodeOf :: Char -> Catcode
quoteCatcodeOf ch
  | isAlpha ch = Letter
  | isSpace ch = Space
  | otherwise = Other

-- | Test whether character @ch@ has catcode @cc@ relative to catcode
-- table @t@.
hasCatcode :: Catcode -> CatcodeTable -> Char -> Bool
hasCatcode cc t ch = catcodeOf ch t == cc

---------- Catcode table modifications (write)

-- | Add a @(Char, Catcode)@ mapping to a 'CatcodeTable'.
updateCatcodeTable :: (Char, Catcode) -> CatcodeTable -> CatcodeTable
updateCatcodeTable = (:)
