----------------------------------------------------------------------
-- |
-- Module      :  Text.TeX.Filter
-- Copyright   :  2015-2016 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  GHC
--
-- Filters for normalizing whitespace and for resolving syntactic
-- TeX and LaTeX macros (symbols and diacritics).
----------------------------------------------------------------------

module Text.TeX.Filter
  ( -- * Normalization
    normalize
    -- * Symbols and diacritics
    -- ** Resolver functions
  , resolveSymbols
    -- ** Data maps
  , SymbolMap
  , symbols
  , diacritics
  , dbldiacritics
  ) where

import Data.Char (isMark)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import qualified Text.TeX.Filter.Plain as Plain
import Text.TeX.Parser.Types


---------- Types

-- | Data map from TeX command names to Unicode symbols.
type SymbolMap = Map String String


---------- Data

-- | A map that contains all registered symbols.
symbols :: SymbolMap
symbols = M.unions
  [ Plain.symbols
  ]

-- | A map that contains all registered diacritics.
diacritics :: SymbolMap
diacritics = M.unions
  [ Plain.diacritics
  ]

-- | A map that contains all registered double diacritics.
dbldiacritics :: SymbolMap
dbldiacritics = M.unions
  [ Plain.dbldiacritics
  ]


---------- Resolve symbols

-- | Resolve symbols and diacritics.
resolveSymbols :: TeX -> TeX
resolveSymbols = map (resolve (symbols, diacritics))

resolve :: (SymbolMap, SymbolMap) -> TeXAtom -> TeXAtom
resolve db@(symdb, accdb) (Command name args) =
  case M.lookup name symdb of
    Just str -> Plain str
    Nothing -> case M.lookup name accdb of
      Just str ->
        -- Nested diacritics need to be processed bottom-up.
        let target = map (resolve db) (getOblArg 0 args)
        in wrapAsAtom (insertAccent str target)
      Nothing -> Command name (fmapArgs (map (resolve db)) args)
resolve db atom = fmapAtom (map (resolve db)) atom

-- Insert combining accent after the first character and any following
-- Unicode mark characters (determined by 'isMark').
insertAccent :: String -> TeX -> TeX
insertAccent acc [] = [Plain (' ':acc)]
insertAccent acc (x:xs) = insertAccentInAtom acc x : xs

insertAccentInAtom :: String -> TeXAtom -> TeXAtom
insertAccentInAtom acc (Plain []) = Plain (' ':acc)
insertAccentInAtom acc (Plain (x:xs)) =
  let (marks, trailer) = span isMark xs
  in Plain (x: marks ++ acc ++ trailer)
insertAccentInAtom _ cmd@Command{} = cmd
insertAccentInAtom acc (Group name args body) = Group name args (insertAccent acc body)
insertAccentInAtom acc (MathGroup mtype body) = MathGroup mtype (insertAccent acc body)
insertAccentInAtom acc (SupScript body) = SupScript (insertAccent acc body)
insertAccentInAtom acc (SubScript body) = SubScript (insertAccent acc body)
insertAccentInAtom _ AlignMark = AlignMark
insertAccentInAtom acc White = Plain (' ':acc)
insertAccentInAtom _ Newline = Newline
insertAccentInAtom _ Par = Par

-- Package 'TeX' as 'TeXAtom'.
wrapAsAtom :: TeX -> TeXAtom
wrapAsAtom [] = Group "" [] []
wrapAsAtom [x] = x
wrapAsAtom xs@(_:_) = Group "" [] xs


---------- Normalization

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


---------- Helpers for mapping over 'TeX' structures

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

-- Lift a 'TeX' function to an 'Args' function.
fmapArgs :: (TeX -> TeX) -> Args -> Args
fmapArgs f = map (fmapArg f)

-- Lift a 'TeX' function to an 'Arg' function.
fmapArg :: (TeX -> TeX) -> Arg -> Arg
fmapArg f (OblArg xs) = OblArg (f xs)
fmapArg f (OptArg xs) = OptArg (f xs)
