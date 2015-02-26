----------------------------------------------------------------------
-- |
-- Module      :  Text.TeX.Lexer.Macro
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Types and utility functions for TeX macros.
----------------------------------------------------------------------

module Text.TeX.Lexer.Macro
  ( -- * Macro types
    Macro
  , macroContext
  , macroBody
    -- * Macro expansion
  , applyMacro
  ) where

import Text.TeX.Lexer.Token (Token(..))

-------------------- Macro types

-- Legacy synonyms.
--type MacroName = String
--type MacroContext = [Token]
--type MacroBody = [Token]

-- For now we use a simple type synonym rather than a full data type
-- so we can use @lookup@ in @[Macro]@ without any unwrapping.
-- Fields: @((name, active), (context, body))@.
-- | A Macro maps a name (and a flag for active characters) to a macro
-- context and a macro body.
type Macro = ((String, Bool), ([Token], [Token]))

-- | Extract context from a macro.
macroContext :: Macro -> [Token]
macroContext = fst . snd

-- | Extract body from a macro.
macroBody :: Macro -> [Token]
macroBody = snd . snd

-------------------- Macro expansion

-- | Substitute variables in macro body.
--
-- Given a macro definition body and a list of actual arguments,
-- substitute the parameter tokens in the macro body by the actual arguments.
applyMacro :: [Token] -> [[Token]] -> [Token]
applyMacro ((Param i n):ts) args = if n == 1
                                   then (args !! (i-1)) ++ applyMacro ts args
                                   else (Param i (n-1)) : applyMacro ts args
applyMacro (tok@(TeXChar _ _):ts) args = tok : applyMacro ts args
applyMacro (tok@(CtrlSeq _ _):ts) args = tok : applyMacro ts args
applyMacro [] _ = []
