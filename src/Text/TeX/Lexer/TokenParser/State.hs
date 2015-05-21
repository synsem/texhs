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
-- Internal state for TeX lexer.
----------------------------------------------------------------------

module Text.TeX.Lexer.TokenParser.State
  ( -- * Types
    LexerStack
  , LexerState
  , defaultLexerStack
  , MacroCmdMap
  , MacroEnvMap
    -- * Accessor functions
    -- ** Group
  , pushGroup
  , popGroup
  , getGroup
  , setGroup
    -- ** Catcode table
  , getCatcodes
  , addCatcode
    -- ** Macro definitions
    -- Generic
--  , getMacros
--  , registerMacro
    -- *** Commands
  , getMacroCmds
  , registerLocalMacroCmd
  , registerMacroCmd
    -- *** Environments
  , getMacroEnvs
  , registerMacroEnv
  ) where

import Text.TeX.Lexer.Catcode
import Text.TeX.Lexer.Macro
import Text.TeX.Lexer.Token


class (Eq k) => Macro k a | k -> a where
  getMacroName :: (k, a) -> String
  getLocalMacros :: LexerState -> [(k, a)]
  addLocalMacro :: (k, a) -> LexerState -> LexerState

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

-- | Stack of open TeX scopes.
-- The head is the current local scope.
type LexerStack = [LexerState]

-- | A TeX scope, with the current catcode table,
-- local macro definitions and the current group type.
data LexerState = LexerState
  { localCatcodes :: CatcodeTable
  , localMacroCmds :: MacroCmdMap
  , localMacroEnvs :: MacroEnvMap
  , localGroup :: Group
  } deriving (Eq, Show)

-- | Create an empty lexer state
-- from a provided expansion mode and group.
emptyLexerState :: Group -> LexerState
emptyLexerState = LexerState [] [] []

-- | The initial lexer stack consists of a single lexer state
-- with the default catcode table and no registered macros.
defaultLexerStack :: LexerStack
defaultLexerStack = LexerState
  { localCatcodes = defaultCatcodeTable
  , localMacroCmds = []
  , localMacroEnvs = []
  , localGroup = AnonymousGroup
  } :[]

---------- Stack manipulation

-- | Push new group onto stack.
pushGroup :: Group -> LexerStack -> LexerStack
pushGroup g = (emptyLexerState g :)

-- | Pop group from stack.
popGroup :: Group -> LexerStack -> LexerStack
popGroup g tl@(_:ls)
  | getGroup tl == g = ls
  | otherwise = error $ "invalid group nesting" ++
                ": expecting " ++ groupEndString (getGroup tl) ++
                " but got " ++ groupEndString g
popGroup _ [] = error "empty lexer stack"

---------- Group

-- | Get current group.
getGroup :: LexerStack -> Group
getGroup (l:_) = localGroup l
getGroup [] = error "empty lexer stack"

-- | Set current group.
setGroup :: Group -> LexerStack -> LexerStack
setGroup g (l:ls) = l {localGroup = g} :ls
setGroup _ [] = error "empty lexer stack"

-- Get string representation of the expected end delimiter for the
-- current group. Only used for error messages.
groupEndString :: Group -> String
groupEndString AnonymousGroup = "}"
groupEndString NativeGroup = "\\endgroup"
groupEndString (NamedGroup name) = "\\end{" ++ show name ++ "}"
groupEndString (DefinedGroup name _ _) = "\\end{" ++ show name ++ "}"

---------- Catcode state

-- | Get current category code table.
getCatcodes :: LexerStack -> CatcodeTable
getCatcodes = concatMap localCatcodes

-- | Add new catcode assignment to current category code table.
addCatcode :: (Char, Catcode) -> LexerStack -> LexerStack
addCatcode ccpair (l:ls) =
  let cctab = updateCatcodeTable ccpair (localCatcodes l)
  in l {localCatcodes = cctab} :ls
addCatcode _ [] = error "empty lexer stack"

---------- Macro state

-- | Get all registered macro definitions.
getMacros :: Macro k a => LexerStack -> [(k, a)]
getMacros = concatMap getLocalMacros

-- | Get all registered macro command definitions.
getMacroCmds :: LexerStack -> MacroCmdMap
getMacroCmds = getMacros

-- | Get all registered environment definitions.
getMacroEnvs :: LexerStack -> MacroEnvMap
getMacroEnvs = getMacros

-- | Register a local macro command definition.
--
-- Prefix macro to the list of currently active macros.
-- The new macro will shadow others with the same name
-- (due to @lookup@'s left bias).
registerLocalMacroCmd :: (MacroCmdKey, MacroCmd) -> LexerStack -> LexerStack
registerLocalMacroCmd m (l:ls) = l {localMacroCmds = m : localMacroCmds l} :ls
registerLocalMacroCmd _ [] = error "empty lexer stack"

-- | Register a global macro definition.
registerGlobalMacro :: Macro k a => (k, a) -> LexerStack -> LexerStack
registerGlobalMacro m ls@(_:_) =
  let (g:tl) = reverse ls
  in reverse (addLocalMacro m g :tl)
registerGlobalMacro _ [] = error "empty lexer stack"

-- | Return whether a macro with the given key is already defined.
macroIsDefined :: Macro k a => k -> LexerStack -> Bool
macroIsDefined m ls = m `elem` (map fst (getMacros ls))

-- | Register a global macro definition.
--
-- The second argument is a flag that indicates whether
-- the macro is already defined.
--
-- This will trigger, depending on 'MacroDefinitionMode',
-- one of three possible actions: register, error, pass (ignore).
registerMacro :: Macro k a => MacroDefinitionMode -> (k, a) ->
                 LexerStack -> LexerStack
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
                    LexerStack -> LexerStack
registerMacroCmd = registerMacro

-- | Register a global macro environment definition.
registerMacroEnv :: MacroDefinitionMode -> (MacroEnvKey, MacroEnv) ->
                    LexerStack -> LexerStack
registerMacroEnv = registerMacro
