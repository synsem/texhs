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
import Control.Applicative ((<*), (<*>), (*>), (<$), (<$>))
#endif
import Control.Monad (void)
import Text.Parsec
  ((<|>), choice, option, optional, many, many1, manyTill,
   count, between, (<?>), eof)

import Text.TeX.Lexer.Catcode
import Text.TeX.Lexer.Token
import Text.TeX.Parser.Core
import Text.TeX.Parser.Types


-- | Main 'TeX' document parser.
texParser :: TeXParser TeX
texParser = atoms <* eof

-- | Parse any number of TeXAtoms.
atoms :: TeXParser TeX
atoms = many atom

-- | Parse a single TeXAtom.
atom :: TeXParser TeXAtom
atom = choice [plain, group, command, white, alignMark,
               mathgroup, subscript, supscript]

-- | Parse subscripted content.
subscript :: TeXParser TeXAtom
subscript = SubScript <$> (charCC Subscript *> arg)

-- | Parse superscripted content.
supscript :: TeXParser TeXAtom
supscript = SupScript <$> (charCC Supscript *> arg)

-- | Parse an align tab.
alignMark :: TeXParser TeXAtom
alignMark = AlignMark <$ charCC AlignTab

-- | Parse a control sequence and return its name.
ctrlseq :: TeXParser String
ctrlseq = getName <$> satisfy isCtrlSeq

-- | Parse a single character irrespective of its catcode.
char :: Char -> TeXParser TeXAtom
char c = Plain [c] <$ satisfy (isCharSat (== c))

-- | Parse a character with the specified catcode.
charCC :: Catcode -> TeXParser Token
charCC = satisfy . hasCC

-- | Parse a single letter or other.
plainChar :: TeXParser TeXAtom
plainChar = (Plain . map getRawChar) <$> count 1 (charCC Letter <|> charCC Other)

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
white = White <$ many1 (charCC Space <|> charCC Eol)

-- | Parse a delimiter that opens a TeX group (begin group).
bgroup :: TeXParser ()
bgroup = void $ charCC Bgroup

-- | Parse a delimiter that closes a TeX group (end group).
egroup :: TeXParser ()
egroup = void $ charCC Egroup

-- | Parse an anonymous TeX group.
group :: TeXParser TeXAtom
group = Group "" ([],[]) <$> between bgroup egroup atoms

-- | Parse a TeX math group.
mathgroup :: TeXParser TeXAtom
mathgroup = charCC Mathshift *> (displaymath <|> inlinemath)

inlinemath :: TeXParser TeXAtom
inlinemath = MathGroup MathInline <$> mathInner
             <* charCC Mathshift <?> "end of inline math"

displaymath :: TeXParser TeXAtom
displaymath = MathGroup MathDisplay <$> (charCC Mathshift *> mathInner)
              <* count 2 (charCC Mathshift) <?> "end of display math"

-- Parse the body of a math group.
-- Like 'atom' but without 'mathgroup'.
mathInner :: TeXParser [TeXAtom]
mathInner = many $ choice [plain, group, command, white, alignMark,
                           subscript, supscript]

-- Use this parser if you know the control sequence requires an argument.
-- | Parse a mandatory argument: the content of a group or a single atom.
arg :: TeXParser TeX
arg = mandarg <|> count 1 plainChar

-- Use this parser as a heuristic to eat up possible mandatory
-- arguments of unknown control sequences.
-- | Parse a mandatory group argument: the content of a group.
mandarg :: TeXParser TeX
mandarg = between bgroup egroup atoms

-- | Parse a traditional-style optional argument in square brackets.
optarg :: TeXParser TeX
optarg = (char '[') *> manyTill optargInner (char ']')

-- Like 'atom' but stops on square brackets.
-- (Stub. Use 'balanced' parsers.)
optargInner :: TeXParser TeXAtom
optargInner = choice [plainExcept "[]", group, command, white, alignMark,
                      mathgroup, subscript, supscript]

-- | Parse a control sequence.
command :: TeXParser TeXAtom
command = ctrlseq >>= processCtrlseq

processCtrlseq :: String -> TeXParser TeXAtom
processCtrlseq name = case name of
  "par" -> return Par
  "\\" -> optional optarg *> return Newline
  "begin" -> env
  _ -> do
    args <- maybe parseUnknownArgSpec parseArgSpec (lookup name commandDB)
    return (Command name args)

-- Lookup table for the ArgSpec of known commands. Stub.
-- (To be replaced by a more general external database.)
--
-- We use a pair @(nrOptArgs, nrMandArgs)@ as a simplified
-- ArgSpec representation here.
commandDB :: [(String, (Int, Int))]
commandDB =
  [ ("rm", (0,0))
  , ("it", (0,0))
  , ("textrm", (0,1))
  , ("textit", (0,1))
  ]

-- Lookup table for the ArgSpec of known environments. Stub.
envDB :: [(String, (Int, Int))]
envDB =
  [ ("tabular", (1,1))
  ]

-- Parse a given ArgSpec.
parseArgSpec :: (Int, Int) -> TeXParser Args
parseArgSpec (nrOpt, nrMand) = do
  opt <- count nrOpt (option [] optarg)
  mand <- count nrMand arg
  return (opt, mand)

-- Heuristic for arguments of unknown control sequences: consume any
-- number of subsequent tokens that look like optional or mandatory
-- arguments, think: ([...])*({...})*.
parseUnknownArgSpec :: TeXParser Args
parseUnknownArgSpec = do
  opt <- many optarg
  mand <- many mandarg
  return (opt, mand)

-- | Parse the name of an environment.
envName :: TeXParser String
envName = map getRawChar <$> between bgroup egroup
          (many1 (charCC Letter <|> charCC Other))

-- | Parse an environment.
env :: TeXParser TeXAtom
env = do
  name <- envName
  args <- maybe parseUnknownArgSpec parseArgSpec (lookup name envDB)
  body <- envInner name
  return (Group name args body)

-- Parse the body of an environment.
envInner :: String -> TeXParser [TeXAtom]
envInner name = commandEnvInner name <|> ((:) <$>
  -- like 'atom' but without 'command'
  choice [plain, group, white, alignMark,
          mathgroup, subscript, supscript] <*>
  envInner name)

-- Parse a command inside the body of an environment.
commandEnvInner :: String -> TeXParser [TeXAtom]
commandEnvInner name = do
  cname <- ctrlseq
  if cname == "end"
    then between bgroup egroup
         (mapM_ char name <?> "\\end{" ++ name ++ "}")
         *> return []
    else (:) <$> processCtrlseq cname <*> envInner name
