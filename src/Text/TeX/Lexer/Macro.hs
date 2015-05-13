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
-- Types and utility functions for TeX macros and environments.
----------------------------------------------------------------------

module Text.TeX.Lexer.Macro
  ( -- * Macro types
    Macro
  , MacroKey
  , macroName
  , macroContext
  , macroBody
    -- * Environment types
  , MacroEnv
  , MacroEnvKey
  , MacroEnvDef(..)
    -- * Macro arguments
  , ArgSpec
  , ArgType(..)
    -- * Macro definition modes
  , MacroDefinitionMode(..)
    -- * Macro expansion
  , applyMacro
  ) where

import Text.TeX.Lexer.Token (Token(..))
import Text.TeX.Lexer.Catcode (Catcode)

-------------------- Argument Specification

-- | A list of argument types that a macro consumes.
type ArgSpec = [ArgType]

-- | Possible argument types for macros.
--
-- This mainly corresponds to xparse (LaTeX3) argument types,
-- except for the addition of 'LiteralToken' which facilitates
-- the translation from original TeX macro definitions (\\def).
data ArgType
  -- | For 'm' args.
  = Mandatory
  -- | For 'u' args.
  | Until [Token]
  -- | For 'l' args.
  | UntilCC Catcode
  -- | For 'r' args: Opening delimiter, closing delimiter, default value.
  | Delimited Token Token (Maybe [Token])
  -- | For 'o' and 'd' args: Opening delimiter, closing delimiter, default value.
  | OptionalGroup Token Token (Maybe [Token])
  -- | For 'g' args: Opening delimiter, closing delimiter, default value.
  | OptionalGroupCC Catcode Catcode (Maybe [Token])
  -- | For 's' and 't' args.
  | OptionalToken Token
  -- | Literal token.
  | LiteralToken Token
  deriving (Eq, Show)


-- Fields: @(name, active)@.
-- | Key for macro lookup.
type MacroKey = (String, Bool)

-- For now we use a simple type synonym rather than a full data type
-- so we can use @lookup@ in @[Macro]@ without any unwrapping.
-- Fields: @((name, active), (context, body))@.
-- | A Macro maps a name (and a flag for active characters) to a macro
-- context and a macro body.
type Macro = (MacroKey, (ArgSpec, [Token]))

-- | Extract name of a macro.
macroName :: Macro -> String
macroName = fst . fst

-- | Extract context from a macro.
macroContext :: Macro -> ArgSpec
macroContext = fst . snd

-- | Extract body from a macro.
macroBody :: Macro -> [Token]
macroBody = snd . snd

-------------------- MacroEnv types

-- | Key for environment lookup.
type MacroEnvKey = [Token]

-- | Definition of an environment.
data MacroEnvDef = MacroEnvDef
                   { macroEnvContext :: ArgSpec
                   , macroEnvStart :: [Token]
                   , macroEnvEnd :: [Token]
                   } deriving (Eq, Show)

-- | An environment maps a name to a macro context,
-- a start code (before) and an end code (after).
type MacroEnv = (MacroEnvKey, MacroEnvDef)


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

-------------------- Macro definition modes

-- | Requests for registering a macro can specify whether the macro is
-- allowed or required to overwrite an existing macro with the same
-- name.
data MacroDefinitionMode
  = MacroNew
  | MacroRenew
  | MacroProvide
  | MacroDeclare
