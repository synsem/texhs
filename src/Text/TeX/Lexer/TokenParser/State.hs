{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.TeX.Lexer.TokenParser.State
-- Copyright   :  2015-2016 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  GHC
--
-- The lexer state manages the catcode table, group nesting
-- and user-defined macro commands and environments.
----------------------------------------------------------------------

module Text.TeX.Lexer.TokenParser.State
  ( -- * Types
    LexerState
  , defaultLexerState
  , validate
  , LexerStateError
  , ThrowsError
    -- * Accessor functions
    -- ** Catcodes
  , getCatcodes
  , addCatcode
    -- ** Groups
  , pushGroup
  , popGroup
  , getGroup
  , setGroup
  , clearGroup
  , getGroupEndCode
    -- ** User macros
    -- Generic
--  , lookupMacro
--  , registerMacro
    -- *** Commands
  , lookupMacroCmd
  , lookupMacroCmdUser
  , registerMacroCmd
  , registerLocalMacroCmd
    -- *** Environments
  , lookupMacroEnv
  , registerMacroEnv
    -- ** Meaning
  , getCharMeaning
  , getMacroMeaning
  , meansPrimitive
  ) where

import Text.TeX.Lexer.Catcode
import Text.TeX.Lexer.Macro
import Text.TeX.Lexer.Token

import Control.Monad (guard)
import Data.List (find)

---------- Types

-- | Internal state of the TeX lexer.
newtype LexerState = LexerState {
    getLexerState :: [Scope]
  } deriving (Eq, Show)

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

-- | Raise a lexer state error if the state is invalid.
validate :: LexerState -> ThrowsError LexerState
validate ls
  | isValid ls = Right ls
  | otherwise = throwE "invalid lexer state: too many closing braces?"

-- | Return whether the lexer state is valid.
isValid :: LexerState -> Bool
isValid = not . null . getLexerState

-- | Lexer state error.
newtype LexerStateError = LexerStateError String
  deriving (Eq, Show)

-- | Return type for functions that may fail
-- with a lexer state error.
type ThrowsError = Either LexerStateError

-- Throw a 'LexerStateError'.
throwE :: String -> ThrowsError a
throwE = Left . LexerStateError

---------- Constructors

-- | The initial lexer state contains the default catcode table
-- and no registered macros except for primitive commands.
defaultLexerState :: LexerState
defaultLexerState = LexerState [Scope
  { localCatcodes = defaultCatcodeTable
  , localMacroCmds = defaultPrimitives
  , localMacroEnvs = []
  , localGroup = AnonymousGroup
  }]

-- | Create an empty scope from a provided group.
emptyScope :: Group -> Scope
emptyScope = Scope [] [] []

---------- Groups

-- | Push new group.
pushGroup :: Group -> LexerState -> ThrowsError LexerState
pushGroup g = Right . fmapL (emptyScope g:)

-- | Pop matching group.
-- Throws an error on group mismatch (invalid nesting).
popGroup :: Group -> LexerState -> ThrowsError LexerState
popGroup g (LexerState (l:ls))
  | localGroup l == g = Right $ LexerState ls
  | otherwise = throwE $ "invalid group nesting" ++
                ": expecting " ++ groupEndString (localGroup l) ++
                " but got " ++ groupEndString g
popGroup _ _ = throwE "invalid lexer state"

-- Note: This is a partial function but the error condition can
-- never be reached if the lexer state is accessed via 'getState'
-- which makes sure that the lexer state is valid (not null).
-- | Get current group.
getGroup :: LexerState -> Group
getGroup (LexerState (l:_)) = localGroup l
getGroup _ = error "invalid lexer state"

-- | Set current group.
setGroup :: Group -> LexerState -> ThrowsError LexerState
setGroup g (LexerState (l:ls)) = Right $ LexerState (l {localGroup = g} :ls)
setGroup _ _ = throwE "invalid lexer state"

-- | Clear the code portions stored in a DefinedGroup.
clearGroup :: Group -> LexerState -> ThrowsError LexerState
clearGroup g@DefinedGroup{} (LexerState ls) =
  let (xs,ys) = break ((== g) . localGroup) ls
  in case ys of
    [] -> throwE $ "invalid group nesting: " ++ show g
    (z:zs) ->
      let DefinedGroup name _ _ = localGroup z
      in Right $ LexerState (xs ++ z { localGroup = DefinedGroup name [] []} :zs)
clearGroup _ _ = throwE "invalid call to clearGroup: expecting DefinedGroup"

-- | Retrieve the end code portion stored in a DefinedGroup.
getGroupEndCode :: Group -> LexerState -> [Token]
getGroupEndCode g (LexerState ls) =
  case find (== g) (map localGroup ls) of
    Just (DefinedGroup _ _ endCode) -> endCode
    _ -> []

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
addCatcode :: (Char, Catcode) -> LexerState -> ThrowsError LexerState
addCatcode ccpair (LexerState (l:ls)) =
  let cctab = updateCatcodeTable ccpair (localCatcodes l)
  in Right $ LexerState (l {localCatcodes = cctab} :ls)
addCatcode _ _ = throwE "invalid lexer state"

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

-- | Lookup macro command definition (user-defined or primitive).
lookupMacroCmd :: MacroCmdKey -> LexerState -> Maybe MacroCmd
lookupMacroCmd = lookupMacro

-- | Lookup macro command definition,
-- restricted to user-defined macros ('MacroCmdUser').
lookupMacroCmdUser :: MacroCmdKey -> LexerState -> Maybe MacroCmd
lookupMacroCmdUser k l = lookupMacroCmd k l >>= \m ->
  guard (isMacroCmdUser m) >> return m

-- | Lookup macro environment definition.
lookupMacroEnv :: MacroEnvKey -> LexerState -> Maybe MacroEnv
lookupMacroEnv = lookupMacro

-- | Get all registered macro definitions.
getMacros :: Macro k a => LexerState -> [(k, a)]
getMacros = concatMap getLocalMacros . getLexerState

-- | Register a local macro command definition.
registerLocalMacroCmd :: (MacroCmdKey, MacroCmd) -> LexerState -> ThrowsError LexerState
registerLocalMacroCmd m (LexerState (l:ls)) =
  Right $ LexerState (l {localMacroCmds = m : localMacroCmds l} :ls)
registerLocalMacroCmd _ _ = throwE "invalid lexer state"

-- | Register a global macro definition.
registerGlobalMacro :: Macro k a => (k, a) -> LexerState -> ThrowsError LexerState
registerGlobalMacro m (LexerState sc@(_:_)) =
  let (g:tl) = reverse sc
  in Right $ LexerState (reverse (addLocalMacro m g :tl))
registerGlobalMacro _ _ = throwE "invalid lexer state"

-- | Return whether a macro with the given key is already defined.
macroIsDefined :: Macro k a => k -> LexerState -> Bool
macroIsDefined m ls = m `elem` map fst (getMacros ls)

-- | Register a global macro definition.
--
-- This will trigger, depending on 'MacroDefinitionMode',
-- one of three possible actions: register, error, pass (ignore).
registerMacro :: Macro k a => MacroDefinitionMode -> (k, a) ->
                 LexerState -> ThrowsError LexerState
registerMacro mode m@(k,_) st
  | macroIsDefined k st = case mode of
    MacroDeclare -> registerGlobalMacro m st
    MacroNew -> throwE $
      "macro already defined" ++ getMacroName m
    MacroRenew -> registerGlobalMacro m st
    MacroProvide -> Right st
  | otherwise = case mode of
    MacroDeclare -> registerGlobalMacro m st
    MacroNew -> registerGlobalMacro m st
    MacroRenew -> throwE $
      "cannot redefine undefined macro: " ++ getMacroName m
    MacroProvide -> registerGlobalMacro m st

-- | Register a global macro command definition.
registerMacroCmd :: MacroDefinitionMode -> (MacroCmdKey, MacroCmd) ->
                    LexerState -> ThrowsError LexerState
registerMacroCmd = registerMacro

-- | Register a global macro environment definition.
registerMacroEnv :: MacroDefinitionMode -> (MacroEnvKey, MacroEnv) ->
                    LexerState -> ThrowsError LexerState
registerMacroEnv = registerMacro

---------- Meanings

-- | Lookup the current meaning of the provided character.
getCharMeaning :: LexerState -> Char -> Meaning
getCharMeaning l ch = MeaningChar ch (catcodeOf ch (getCatcodes l))

-- | Lookup the current meaning of the provided control sequence.
getMacroMeaning :: LexerState -> MacroCmdKey -> Meaning
getMacroMeaning l k = maybe MeaningUndef MeaningMacro $ lookupMacroCmd k l

-- | Test whether the meaning of a control sequence equals a certain primitive.
meansPrimitive :: LexerState -> MacroCmdKey -> Primitive -> Bool
meansPrimitive l k p = isPrimitive p $ getMacroMeaning l k
