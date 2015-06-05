{-# LANGUAGE CPP #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.TeX.Parser.Basic
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Basic TeX parsers.
----------------------------------------------------------------------

module Text.TeX.Parser.Basic
 ( -- * Main document parser
   texParser
 ) where


#if MIN_VERSION_base(4,8,0)
-- Prelude exports all required operators from Control.Applicative
#else
import Control.Applicative ((<*), (*>), (<$), (<$>))
#endif
import Control.Monad (void)
import Text.Parsec ((<|>), many, many1, manyTill, count, between, parserFail, eof)

import Text.TeX.Lexer.Catcode
import Text.TeX.Lexer.Macro
import Text.TeX.Lexer.Token
import Text.TeX.Parser.Core
import Text.TeX.Parser.Types


-- | Main 'TeX' document parser.
texParser :: TeXParser TeX
texParser = atoms <* eof

-- | Parse any number of TeXAtoms.
atoms :: TeXParser TeX
atoms = many atom

-- Stub.
-- | Parse a single TeXAtom.
atom :: TeXParser TeXAtom
atom = failOnParam <|> command <|> white <|> group <|> plain

-- The 'Token' input stream must not contain any 'Param' elements.
-- We enforce this restriction by raising an error if we encounter
-- a 'Param' element.
failOnParam :: TeXParser a
failOnParam = satisfy isParam *> parserFail "encountered a Param element"

-- | Parse a control sequence and return its name.
ctrlseq :: TeXParser String
ctrlseq = getName <$> satisfy isCtrlSeq

-- | Parse a single character irrespective of its catcode.
char :: Char -> TeXParser TeXAtom
char c = Plain [c] <$ satisfy (isCharSat (== c))

-- | Parse a character with the specified catcode.
charCC :: Catcode -> TeXParser Token
charCC = satisfy . hasCC

-- | Parse letters and others.
plain :: TeXParser TeXAtom
plain = (Plain . map getRawChar) <$> many1 (charCC Letter <|> charCC Other)

-- Parse letters and others except for the specified characters.
plainExcept :: [Char] -> TeXParser TeXAtom
plainExcept xs = (Plain . map getRawChar) <$> many1 (satisfy (\t ->
  (hasCC Letter t || hasCC Other t) &&
  not (isCharSat (`elem` xs) t)))

-- | Parse intra-paragraph whitespace.
white :: TeXParser TeXAtom
white = White <$ many1 (satisfy (hasCC Space) <|> satisfy (hasCC Eol))

-- | Parse a delimiter that opens a TeX group (begin group).
bgroup :: TeXParser ()
bgroup = void $ satisfy (hasCC Bgroup)

-- | Parse a delimiter that closes a TeX group (end group).
egroup :: TeXParser ()
egroup = void $ satisfy (hasCC Egroup)

-- | Parse an anonymous TeX group.
group :: TeXParser TeXAtom
group = Group "" ([],[]) <$> between bgroup egroup atoms

-- Use this parser if you know the control sequence requires an argument.
-- | Parse a mandatory argument: the content of a group or a single atom.
arg :: TeXParser TeX
arg = mandarg <|> count 1 atom

-- Use this parser as a heuristic to eat up possible mandatory
-- arguments of unknown control sequences.
-- | Parse a mandatory group argument: the content of a group.
mandarg :: TeXParser TeX
mandarg = between bgroup egroup atoms

-- | Parse a traditional-style optional argument in square brackets.
optarg :: TeXParser TeX
optarg = (char '[') *> manyTill optargInner (char ']')

-- Like 'atom' but stops on square brackets.
optargInner :: TeXParser TeXAtom
optargInner = failOnParam <|> command <|> white <|> group <|> plainExcept "[]"

-- | Parse a control sequence.
command :: TeXParser TeXAtom
command = do
  name <- ctrlseq
  (opt, mand) <- maybe parseUnknownArgSpec parseArgSpec (lookup name commandDB)
  return (Command name (opt, mand))

-- Lookup table for the ArgSpec of known commands. Stub.
-- (To be replaced by a more general external database.)
commandDB :: [(String, ArgSpec)]
commandDB =
  [ ("rm", [])
  , ("it", [])
  , ("textrm", [Mandatory])
  , ("textit", [Mandatory])
  ]

-- Parse a given ArgSpec. Stub.
parseArgSpec :: ArgSpec -> TeXParser Args
parseArgSpec xs = do
  let nrMand = length $ takeWhile (== Mandatory) xs
  mand <- count nrMand arg
  return ([], mand)

-- Heuristic for arguments of unknown control sequences: consume any
-- number of subsequent tokens that look like optional or mandatory
-- arguments, think: ([...])*({...})*.
parseUnknownArgSpec :: TeXParser Args
parseUnknownArgSpec = do
  opt <- many optarg
  mand <- many mandarg
  return (opt, mand)
