----------------------------------------------------------------------
-- |
-- Module      :  Text.TeX.Parser
-- Copyright   :  2015-2017 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
--
-- Maintainer  :  mschenner.dev@gmail.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- Parser for TeX token streams.
----------------------------------------------------------------------

module Text.TeX.Parser
  ( -- * TeX Parser
    parseTeX
  ) where

import Text.TeX.Filter (normalize, resolveSyntacticTeX)
import Text.TeX.Lexer.Token (Token)
import Text.TeX.Parser.Types (TeX)
import Text.TeX.Parser.Core (runTeXParser)
import Text.TeX.Parser.Basic (texParser)


-- | Run 'TeX' parser on named input 'Token' stream.
parseTeX :: String -> [Token] -> TeX
parseTeX name input = case runTeXParser texParser name input of
  Left l  -> error (show l)
  Right r -> (normalize . resolveSyntacticTeX) r
