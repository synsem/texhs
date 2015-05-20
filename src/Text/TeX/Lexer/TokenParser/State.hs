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
    -- ** Catcode table
  , getCatcodes
  , addCatcode
    -- ** Macro definitions
  , getMacroCmds
  , macroCmdIsDefined
  , registerLocalMacroCmd
  , macroCmdDefinitionAction
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
  , localMacroCmds :: [MacroCmd]
  , localMacroEnvs :: [MacroEnv]
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

-- | Get all registered macro command definitions.
getMacroCmds :: LexerStack -> [MacroCmd]
getMacroCmds = concatMap localMacroCmds

-- | Get all registered environment definitions.
getMacroEnvs :: LexerStack -> [MacroEnv]
getMacroEnvs = concatMap localMacroEnvs

-- | Register a local macro command definition.
--
-- Prefix macro to the list of currently active macros.
-- The new macro will shadow others with the same name
-- (due to @lookup@'s left bias).
registerLocalMacroCmd :: MacroCmd -> LexerStack -> LexerStack
registerLocalMacroCmd m (l:ls) = l {localMacroCmds = m : localMacroCmds l} :ls
registerLocalMacroCmd _ [] = error "empty lexer stack"

-- Add macro command definition to the global context.
registerGlobalMacroCmd :: MacroCmd -> LexerStack -> LexerStack
registerGlobalMacroCmd m ls@(_:_) =
  let (g:tl) = reverse ls
  in reverse (g {localMacroCmds = m : localMacroCmds g} :tl)
registerGlobalMacroCmd _ [] = error "empty lexer stack"

-- Add macro environment definition to the global context.
-- Note: All user-defined environments have global scope.
registerGlobalMacroEnv :: MacroEnv -> LexerStack -> LexerStack
registerGlobalMacroEnv m ls@(_:_) =
  let (g:tl) = reverse ls
  in reverse (g {localMacroEnvs = m : localMacroEnvs g} :tl)
registerGlobalMacroEnv _ [] = error "empty lexer stack"

-- | Return whether a macro with the given key is already defined.
macroCmdIsDefined :: MacroCmdKey -> LexerStack -> Bool
macroCmdIsDefined m ls = m `elem` (map fst (getMacroCmds ls))

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
macroCmdDefinitionAction :: MacroDefinitionMode -> Bool ->
                            MacroCmd -> LexerStack -> LexerStack
macroCmdDefinitionAction MacroDeclare _ = registerGlobalMacroCmd
macroCmdDefinitionAction MacroNew True =
  error . (++) "macro already defined: " . show . macroCmdName
macroCmdDefinitionAction MacroNew False = registerGlobalMacroCmd
macroCmdDefinitionAction MacroRenew True = registerGlobalMacroCmd
macroCmdDefinitionAction MacroRenew False =
  error . (++) "cannot redefine undefined macro: " . show . macroCmdName
macroCmdDefinitionAction MacroProvide True = flip const
macroCmdDefinitionAction MacroProvide False = registerGlobalMacroCmd

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
