----------------------------------------------------------------------
-- |
-- Module      :  Text.TeX.Filter.Plain
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Data maps containing symbols and accents defined in the
-- Plain TeX format.
----------------------------------------------------------------------

module Text.TeX.Filter.Plain
  ( symbols
  , accents
  ) where

import Data.Map.Strict (Map, fromList)


-- | Symbols defined in the Plain TeX format.
--
-- We do not distinguish text mode and math mode at this point.
symbols :: Map String String
symbols = fromList
  [ -- text symbols
    ("dag", "\x2020")         -- DAGGER
  , ("i", "\x0131")           -- LATIN SMALL LETTER DOTLESS I
  , ("j", "\x0237")           -- LATIN SMALL LETTER DOTLESS J
    -- math symbols
  , ("dagger", "\x2020")      -- DAGGER
  ]

-- | Accents defined in the Plain TeX format.
--
-- We do not distinguish text mode and math mode at this point.
accents :: Map String String
accents = fromList
  [ -- text accents
    ("\"", "\x0308")          -- COMBINING DIAERESIS
  , ("^",  "\x0302")          -- COMBINING CIRCUMFLEX ACCENT
    -- math accents
  , ("ddot", "\x0308")        -- COMBINING DIAERESIS
  , ("hat",  "\x0302")        -- COMBINING CIRCUMFLEX ACCENT
  ]
