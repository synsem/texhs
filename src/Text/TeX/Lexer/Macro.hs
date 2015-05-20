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
  ( -- * Macro commands
    MacroCmd
  , MacroCmdKey
  , MacroCmdDef(..)
  , macroCmdName
    -- * Macro environments
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

-------------------- Macro Commands

-- | A macro command maps a macro key to its definition.
type MacroCmd = (MacroCmdKey, MacroCmdDef)

-- | Key for macro command lookup: name and active flag.
type MacroCmdKey = (String, Bool)

-- | Definition of a macro command.
data MacroCmdDef = MacroCmdDef
  { macroCmdContext :: ArgSpec
  , macroCmdBody :: [Token]
  } deriving (Eq, Show)

-- | Extract name of a macro command.
macroCmdName :: MacroCmd -> String
macroCmdName = fst . fst

-------------------- Macro Environments

-- | A macro environment maps an environment key to its definition.
type MacroEnv = (MacroEnvKey, MacroEnvDef)

-- | Key for environment lookup.
type MacroEnvKey = [Token]

-- | Definition of an environment.
data MacroEnvDef = MacroEnvDef
  { macroEnvContext :: ArgSpec
  , macroEnvStart :: [Token]
  , macroEnvEnd :: [Token]
  } deriving (Eq, Show)

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
