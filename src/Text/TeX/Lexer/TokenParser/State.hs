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
    -- * Accessor functions
    -- ** Group
  , pushGroup
  , popGroup
  , getGroup
  , setGroup
    -- ** Expansion mode
  , getExpandMode
  , setExpandMode
    -- ** Catcode table
  , getCatcodes
  , addCatcode
    -- ** Macro definitions
  , getMacros
  , macroIsDefined
  , registerLocalMacro
  , macroDefinitionAction
    -- ** Environment definitions
  , getMacroEnvs
  , macroEnvIsDefined
  , macroEnvDefinitionAction
  ) where


import Text.TeX.Lexer.Catcode
import Text.TeX.Lexer.Macro
import Text.TeX.Lexer.Token


-- | Stack of open TeX scopes.
-- The head is the current local scope.
type LexerStack = [LexerState]

-- | A TeX scope, with the current catcode table,
-- local macro definitions and the current group type.
data LexerState = LexerState
  { localCatcodes :: CatcodeTable
  , localMacros :: [Macro]
  , localMacroEnvs :: [MacroEnv]
  , localExpandMode :: Bool
  , localGroup :: Group
  } deriving (Eq, Show)

-- | Create an empty lexer state
-- from a provided expansion mode and group.
emptyLexerState :: Bool -> Group -> LexerState
emptyLexerState = LexerState [] [] []

-- | The initial lexer stack consists of a single lexer state
-- with the default catcode table and no registered macros.
defaultLexerStack :: LexerStack
defaultLexerStack = LexerState
  { localCatcodes = defaultCatcodeTable
  , localMacros = []
  , localMacroEnvs = []
  , localExpandMode = True
  , localGroup = AnonymousGroup
  } :[]

---------- Stack manipulation

-- | Push new group onto stack.
pushGroup :: Group -> LexerStack -> LexerStack
pushGroup g tl@(l:_) = emptyLexerState (localExpandMode l) g :tl
pushGroup _ [] = error "empty lexer stack"

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

---------- Expansion mode

-- | Get current expansion mode.
getExpandMode :: LexerStack -> Bool
getExpandMode (l:_) = localExpandMode l
getExpandMode [] = error "empty lexer stack"

-- | Set current expansion mode.
setExpandMode :: Bool -> LexerStack -> LexerStack
setExpandMode b (l:ls) = l {localExpandMode = b} :ls
setExpandMode _ [] = error "empty lexer stack"

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
getMacros :: LexerStack -> [Macro]
getMacros = concatMap localMacros

-- | Get all registered environment definitions.
getMacroEnvs :: LexerStack -> [MacroEnv]
getMacroEnvs = concatMap localMacroEnvs

-- | Register a local macro definition.
--
-- Prefix macro to the list of currently active macros.
-- The new macro will shadow others with the same name
-- (due to @lookup@'s left bias).
registerLocalMacro :: Macro -> LexerStack -> LexerStack
registerLocalMacro m (l:ls) = l {localMacros = m : localMacros l} :ls
registerLocalMacro _ [] = error "empty lexer stack"

-- Add macro definition to the global context.
registerGlobalMacro :: Macro -> LexerStack -> LexerStack
registerGlobalMacro m ls@(_:_) =
  let (g:tl) = reverse ls
  in reverse (g {localMacros = m : localMacros g} :tl)
registerGlobalMacro _ [] = error "empty lexer stack"

-- Add environment definition to the global context.
-- Note: All user-defined environments have global scope.
registerGlobalMacroEnv :: MacroEnv -> LexerStack -> LexerStack
registerGlobalMacroEnv m ls@(_:_) =
  let (g:tl) = reverse ls
  in reverse (g {localMacroEnvs = m : localMacroEnvs g} :tl)
registerGlobalMacroEnv _ [] = error "empty lexer stack"

-- | Return whether a macro with the given key is already defined.
macroIsDefined :: MacroKey -> LexerStack -> Bool
macroIsDefined m ls = m `elem` (map fst (getMacros ls))

-- | Return whether an environment with the given key is already defined.
macroEnvIsDefined :: MacroEnvKey -> LexerStack -> Bool
macroEnvIsDefined m ls = m `elem` (map fst (getMacroEnvs ls))

-- | Register a global macro definition.
--
-- The second argument is a flag that indicates whether
-- the macro is already defined.
--
-- This will trigger, depending on 'MacroDefinitionMode',
-- one of three possible actions: register, error, pass (ignore).
macroDefinitionAction :: MacroDefinitionMode -> Bool ->
                         Macro -> LexerStack -> LexerStack
macroDefinitionAction MacroDeclare _ = registerGlobalMacro
macroDefinitionAction MacroNew True =
  error . (++) "macro already defined: " . show . macroName
macroDefinitionAction MacroNew False = registerGlobalMacro
macroDefinitionAction MacroRenew True = registerGlobalMacro
macroDefinitionAction MacroRenew False =
  error . (++) "cannot redefine undefined macro: " . show . macroName
macroDefinitionAction MacroProvide True = flip const
macroDefinitionAction MacroProvide False = registerGlobalMacro

-- | Register a global environment definition.
--
-- The second argument is a flag that indicates whether
-- the macro is already defined.
--
-- This will trigger, depending on 'MacroDefinitionMode',
-- one of three possible actions: register, error, pass (ignore).
macroEnvDefinitionAction :: MacroDefinitionMode -> Bool ->
                         MacroEnv -> LexerStack -> LexerStack
macroEnvDefinitionAction MacroDeclare _ = registerGlobalMacroEnv
macroEnvDefinitionAction MacroNew True =
  error . (++) "environment already defined: " . show . fst
macroEnvDefinitionAction MacroNew False = registerGlobalMacroEnv
macroEnvDefinitionAction MacroRenew True = registerGlobalMacroEnv
macroEnvDefinitionAction MacroRenew False =
  error . (++) "cannot redefine undefined environment: " . show . fst
macroEnvDefinitionAction MacroProvide True = flip const
macroEnvDefinitionAction MacroProvide False = registerGlobalMacroEnv
