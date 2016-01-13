{-# LANGUAGE CPP #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.TeX.Parser.Basic
-- Copyright   :  2015-2016 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  GHC
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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Text.Parsec
  ((<|>), choice, optional, optionMaybe, many, many1, manyTill,
   count, between, (<?>), parserFail, eof)

import Text.TeX.Filter (argspecsSyntactic)
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
subscript = SubScript <$> (charCC Subscript *> argbody)

-- | Parse superscripted content.
supscript :: TeXParser TeXAtom
supscript = SupScript <$> (charCC Supscript *> argbody)

-- | Parse an align tab.
alignMark :: TeXParser TeXAtom
alignMark = AlignMark <$ charCC AlignTab

-- | Parse a control sequence and return its name.
ctrlseq :: TeXParser String
ctrlseq = getName <$> satisfy isCtrlSeq

-- | Parse an active character and return its name.
activechar :: TeXParser String
activechar = getName <$> satisfy isActiveChar

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
group = Group "" [] <$> groupbody

-- | Parse the content of a group.
groupbody :: TeXParser TeX
groupbody = between bgroup egroup atoms

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
-- (Like 'argbody' but wrapped in 'Arg'.)
-- | Parse an obligatory argument: the content of a group or a single atom.
arg :: TeXParser Arg
arg = OblArg <$> argbody

-- | Parse the content of an obligatory argument:
-- the content of a group or a single atom.
argbody :: TeXParser TeX
argbody = groupbody <|> count 1 plainChar

-- Use this parser as a heuristic to eat up possible obligatory
-- arguments of unknown control sequences.
-- | Parse an obligatory group argument: the content of a group.
oblarg :: TeXParser Arg
oblarg = OblArg <$> groupbody

-- | Parse a traditional-style optional argument in square brackets.
optarg :: TeXParser Arg
optarg = OptArg <$> (char '[' *> manyTill optargInner (char ']'))

-- Like 'atom' but stops on square brackets.
-- (Stub. Use 'balanced' parsers.)
optargInner :: TeXParser TeXAtom
optargInner = choice [plainExcept "[]", group, command, white, alignMark,
                      mathgroup, subscript, supscript]

-- | Parse a control sequence or an active character.
command :: TeXParser TeXAtom
command = commandCtrlseq <|> commandActiveChar

-- | Parse an active character.
commandActiveChar :: TeXParser TeXAtom
commandActiveChar = activechar >>= processActiveChar

-- Interpret an active character.
processActiveChar :: String -> TeXParser TeXAtom
processActiveChar name = case name of
  "~" -> return (Plain "\x00A0") -- NO-BREAK SPACE
  _ -> parserFail ("Encountered unknown active character: " ++ name)

-- | Parse a control sequence.
commandCtrlseq :: TeXParser TeXAtom
commandCtrlseq = ctrlseq >>= processCtrlseq

-- Interpret a control sequence.
processCtrlseq :: String -> TeXParser TeXAtom
processCtrlseq name = case name of
  "par" -> return Par
  " " -> return White  -- control space
  "\n" -> return White -- control newline
  "\\" -> optional optarg *> return Newline
  "begin" -> env
  _ -> do
    args <- maybe parseUnknownArgSpec parseArgSpec (M.lookup name commandDB)
    return (Command name args)

-- Lookup table for the ArgSpec of known commands.
--
-- We use a pair @(nrOptArgs, nrOblArgs)@ as a simplified
-- ArgSpec representation here.
commandDB :: Map String (Int, Int)
commandDB = M.union argspecsSyntactic argspecsSemantic
  where
    argspecsSemantic = M.fromList
      [ -- Plain TeX
        ("rm", (0,0))
      , ("it", (0,0))
        -- LaTeX
      , ("textrm", (0,1))
      , ("textit", (0,1))
        -- hyperref
      , ("href", (0,2))
      , ("url", (0,1))
      ]

-- Lookup table for the ArgSpec of known environments. Stub.
envDB :: [(String, (Int, Int))]
envDB =
  [ ("tabular", (1,1))
  ]

-- Parse a given ArgSpec.
parseArgSpec :: (Int, Int) -> TeXParser Args
parseArgSpec (nrOpt, nrObl) = do
  opt <- catMaybes <$> count nrOpt (optionMaybe optarg)
  obl <- count nrObl arg
  return (opt ++ obl)

-- Heuristic for arguments of unknown control sequences: consume any
-- number of subsequent tokens that look like optional or obligatory
-- arguments, think: ([...])*({...})*.
parseUnknownArgSpec :: TeXParser Args
parseUnknownArgSpec = do
  opt <- many optarg
  obl <- many oblarg
  return (opt ++ obl)

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
  -- like 'atom' but without 'commandCtrlseq' (in 'command')
  choice [plain, group, commandActiveChar, white, alignMark,
          mathgroup, subscript, supscript] <*>
  envInner name)

-- Parse a command (control sequence) inside the body of an environment.
commandEnvInner :: String -> TeXParser [TeXAtom]
commandEnvInner name = do
  cname <- ctrlseq
  if cname == "end"
    then between bgroup egroup
         (mapM_ char name <?> "\\end{" ++ name ++ "}")
         *> return []
    else (:) <$> processCtrlseq cname <*> envInner name
