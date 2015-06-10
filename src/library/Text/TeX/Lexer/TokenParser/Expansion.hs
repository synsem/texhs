{-# LANGUAGE CPP #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.TeX.Lexer.TokenParser.Expansion
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Expansion of user-defined macros and environments.
----------------------------------------------------------------------

module Text.TeX.Lexer.TokenParser.Expansion
  ( -- * Macro expansion
    expand
    -- * Environment expansion
  , expandEnvironment
  ) where

#if MIN_VERSION_base(4,8,0)
-- Prelude exports all required operators from Control.Applicative
#else
import Control.Applicative ((<$), (<$>))
#endif
import Control.Monad ((>=>))
import Data.Maybe (fromMaybe)

import Text.TeX.Lexer.Macro
import Text.TeX.Lexer.Token
import Text.TeX.Lexer.TokenParser.Basic
import Text.TeX.Lexer.TokenParser.Core


-------------------- Macro expansion

-- | Expand a call of a user-defined macro
-- and push the expansion back into the input stream.
expand :: MacroCmd -> Parser ()
expand = expansion >=> prependToInput

-- | Expand a call of a user-defined macro
-- and return the expansion.
expansion :: MacroCmd -> Parser [Token]
expansion m = do
  args <- parseArgspec (macroCmdContext m)
  return $ applyMacro (macroCmdBody m) args

-------------------- Environment expansion

-- | Expand a user-defined environment
-- and return the expansion as a pair of
-- @start code@ and @end code@.
expandEnvironment :: MacroEnv -> Parser ([Token], [Token])
expandEnvironment (MacroEnv _ context startCode endCode) = do
  args <- parseArgspec context
  return (applyMacro startCode args, applyMacro endCode args)

-------------------- Helper functions

-- Parse the arguments in a macro call.
parseArgspec :: ArgSpec -> Parser [[Token]]
parseArgspec = mapM parseArgtype

-- Parse a single argument in a macro call.
parseArgtype :: ArgType -> Parser [Token]
parseArgtype Mandatory = stripBraces <$> tokenNoExpand
parseArgtype (Until [t]) = untilTok t
parseArgtype (Until ts) = untilToks ts
parseArgtype (UntilCC cc) = many (charccno cc)
parseArgtype (Delimited open close defval) =
  option (fromMaybe [noValueTok] defval) (balanced open close)
parseArgtype (OptionalGroup open close defval) =
  option (fromMaybe [noValueTok] defval) (balanced open close)
parseArgtype (OptionalGroupCC open close defval) =
  option (fromMaybe [noValueTok] defval) (balancedCC open close)
parseArgtype (OptionalToken t) =
  option [falseTok] ([trueTok] <$ tok t)
parseArgtype (LiteralToken t) = count 1 (tok t)
