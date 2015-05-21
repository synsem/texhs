{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.TeX.Lexer.TokenParser.State
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  portable
--
-- The lexer state manages the catcode table, group nesting
-- and user-defined macro commands and environments.
----------------------------------------------------------------------

module Text.TeX.Lexer.TokenParser.State
  ( -- * Types
    LexerState
  , defaultLexerState
    -- * Accessor functions
    -- ** Catcodes
  , getCatcodes
  , addCatcode
    -- ** Groups
  , pushGroup
  , popGroup
  , getGroup
  , setGroup
    -- ** User macros
    -- Generic
--  , lookupMacro
--  , registerMacro
    -- *** Commands
  , lookupMacroCmd
  , registerMacroCmd
  , registerLocalMacroCmd
    -- *** Environments
  , lookupMacroEnv
  , registerMacroEnv
  ) where

import Text.TeX.Lexer.Catcode
import Text.TeX.Lexer.Macro
import Text.TeX.Lexer.Token


---------- Types

-- | Internal state of the TeX lexer.
newtype LexerState = LexerState { getLexerState :: [Scope] }
  deriving (Eq, Show)

-- | Map a function over a LexerState.
fmapL :: ([Scope] -> [Scope]) -> LexerState -> LexerState
fmapL f = LexerState . f . getLexerState

-- | A TeX scope, with the current catcode table,
-- local macro definitions and the current group type.
data Scope = Scope
  { localCatcodes :: CatcodeTable
  , localMacroCmds :: MacroCmdMap
  , localMacroEnvs :: MacroEnvMap
  , localGroup :: Group
  } deriving (Eq, Show)

---------- Constructors

-- | The initial lexer state contains the default catcode table
-- and no registered macros.
defaultLexerState :: LexerState
defaultLexerState = LexerState [Scope
  { localCatcodes = defaultCatcodeTable
  , localMacroCmds = []
  , localMacroEnvs = []
  , localGroup = AnonymousGroup
  }]

-- | Create an empty scope from a provided group.
emptyScope :: Group -> Scope
emptyScope = Scope [] [] []

---------- Groups

-- | Push new group.
pushGroup :: Group -> LexerState -> LexerState
pushGroup g = fmapL (emptyScope g:)

-- | Pop matching group.
-- Throws an error on group mismatch (invalid nesting).
popGroup :: Group -> LexerState -> LexerState
popGroup g st@(LexerState (_:ls))
  | getGroup st == g = LexerState ls
  | otherwise = error $ "invalid group nesting" ++
                ": expecting " ++ groupEndString (getGroup st) ++
                " but got " ++ groupEndString g
popGroup _ _ = error "invalid lexer state"

-- | Get current group.
getGroup :: LexerState -> Group
getGroup (LexerState (l:_)) = localGroup l
getGroup _ = error "invalid lexer state"

-- | Set current group.
setGroup :: Group -> LexerState -> LexerState
setGroup g (LexerState (l:ls)) = LexerState (l {localGroup = g} :ls)
setGroup _ _ = error "invalid lexer state"

-- Get string representation of the expected end delimiter for the
-- current group. Only used for error messages.
groupEndString :: Group -> String
groupEndString AnonymousGroup = "}"
groupEndString NativeGroup = "\\endgroup"
groupEndString (NamedGroup name) = "\\end{" ++ show name ++ "}"
groupEndString (DefinedGroup name _ _) = "\\end{" ++ show name ++ "}"

---------- Catcodes

-- | Get current category code table.
getCatcodes :: LexerState -> CatcodeTable
getCatcodes = concatMap localCatcodes . getLexerState

-- | Add new catcode assignment to the local category code table.
addCatcode :: (Char, Catcode) -> LexerState -> LexerState
addCatcode ccpair (LexerState (l:ls)) =
  let cctab = updateCatcodeTable ccpair (localCatcodes l)
  in LexerState (l {localCatcodes = cctab} :ls)
addCatcode _ _ = error "invalid lexer state"

---------- User macros

-- Generalize over macro commands and environments.
class (Eq k) => Macro k a | k -> a where
  getMacroName :: (k, a) -> String
  getLocalMacros :: Scope -> [(k, a)]
  addLocalMacro :: (k, a) -> Scope -> Scope

instance Macro MacroCmdKey MacroCmd where
  getMacroName = fst . macroCmdName . snd
  getLocalMacros = localMacroCmds
  addLocalMacro m ls = ls {localMacroCmds = m : localMacroCmds ls}

instance Macro MacroEnvKey MacroEnv where
  getMacroName = show . macroEnvName . snd
  getLocalMacros = localMacroEnvs
  addLocalMacro m ls = ls {localMacroEnvs = m : localMacroEnvs ls}

-- | Lookup table for macro commands.
type MacroCmdMap = [(MacroCmdKey, MacroCmd)]

-- | Lookup table for macro environments.
type MacroEnvMap = [(MacroEnvKey, MacroEnv)]

-- | Lookup macro definition.
lookupMacro :: Macro k a => k -> LexerState -> Maybe a
lookupMacro k = lookup k . getMacros

-- | Lookup macro command definition.
lookupMacroCmd :: MacroCmdKey -> LexerState -> Maybe MacroCmd
lookupMacroCmd = lookupMacro

-- | Lookup macro environment definition.
lookupMacroEnv :: MacroEnvKey -> LexerState -> Maybe MacroEnv
lookupMacroEnv = lookupMacro

-- | Get all registered macro definitions.
getMacros :: Macro k a => LexerState -> [(k, a)]
getMacros = concatMap getLocalMacros . getLexerState

-- | Register a local macro command definition.
registerLocalMacroCmd :: (MacroCmdKey, MacroCmd) -> LexerState -> LexerState
registerLocalMacroCmd m (LexerState (l:ls)) =
  LexerState (l {localMacroCmds = m : localMacroCmds l} :ls)
registerLocalMacroCmd _ _ = error "invalid lexer state"

-- | Register a global macro definition.
registerGlobalMacro :: Macro k a => (k, a) -> LexerState -> LexerState
registerGlobalMacro m (LexerState sc@(_:_)) =
  let (g:tl) = reverse sc
  in LexerState (reverse (addLocalMacro m g :tl))
registerGlobalMacro _ _ = error "invalid lexer state"

-- | Return whether a macro with the given key is already defined.
macroIsDefined :: Macro k a => k -> LexerState -> Bool
macroIsDefined m ls = m `elem` (map fst (getMacros ls))

-- | Register a global macro definition.
--
-- This will trigger, depending on 'MacroDefinitionMode',
-- one of three possible actions: register, error, pass (ignore).
registerMacro :: Macro k a => MacroDefinitionMode -> (k, a) ->
                 LexerState -> LexerState
registerMacro mode m@(k,_) st
  | macroIsDefined k st = case mode of
    MacroDeclare -> registerGlobalMacro m st
    MacroNew -> error $ "macro already defined" ++ (getMacroName m)
    MacroRenew -> registerGlobalMacro m st
    MacroProvide -> st
  | otherwise = case mode of
    MacroDeclare -> registerGlobalMacro m st
    MacroNew -> registerGlobalMacro m st
    MacroRenew -> error $ "cannot redefine undefined macro: " ++ (getMacroName m)
    MacroProvide -> registerGlobalMacro m st

-- | Register a global macro command definition.
registerMacroCmd :: MacroDefinitionMode -> (MacroCmdKey, MacroCmd) ->
                    LexerState -> LexerState
registerMacroCmd = registerMacro

-- | Register a global macro environment definition.
registerMacroEnv :: MacroDefinitionMode -> (MacroEnvKey, MacroEnv) ->
                    LexerState -> LexerState
registerMacroEnv = registerMacro
