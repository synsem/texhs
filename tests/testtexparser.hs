----------------------------------------------------------------------
-- Tests for the @Text.TeX.Parser@ module.
----------------------------------------------------------------------
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
-- Maintainer  :  mathias.schenner@langsci-press.org
----------------------------------------------------------------------

module Main where

import System.Exit (ExitCode, exitSuccess, exitFailure)
import Test.HUnit (Test(..), Counts(..), test, (~:), (~=?), runTestTT)


import Text.TeX.Lexer.Catcode
import Text.TeX.Lexer.Token (Token(TeXChar,CtrlSeq))
import Text.TeX.Parser (parseTeX)
import Text.TeX.Parser.Types (TeXAtom(..))

-------------------- Token construction helpers

mkLetter :: Char -> Token
mkLetter = flip TeXChar Letter

mkString :: String -> [Token]
mkString = map mkLetter

mkOther :: Char -> Token
mkOther = flip TeXChar Other

mkCtrlSeq :: String -> Token
mkCtrlSeq = flip CtrlSeq False

mkOptArg :: [Token] -> [Token]
mkOptArg = between (mkOther '[') (mkOther ']')

mkGroup :: [Token] -> [Token]
mkGroup = between (TeXChar '{' Bgroup) (TeXChar '}' Egroup)

between :: Token -> Token -> [Token] -> [Token]
between open close content = open : (content ++ [close])

spcTok :: Token
spcTok = TeXChar ' ' Space

eolTok :: Token
eolTok = TeXChar '\n' Eol

-------------------- tests

testsBasic :: Test
testsBasic = TestLabel "basic" $ test
  [ "merge spaces"
    ~: [White]
    ~=? (parseTeX "" [spcTok, eolTok, spcTok])
  , "two words"
    ~: [Plain "hello", White, Plain "world"]
    ~=? (parseTeX "" $
         mkString "hello" ++
         [eolTok, spcTok] ++
         mkString "world")
  , "command with no arguments"
    ~: [Command "hello" ([],[])]
    ~=? (parseTeX "" [mkCtrlSeq "hello"])
  , "command with no arguments between letters"
    ~: [Plain "a", Command "hello" ([],[]), Plain "z"]
    ~=? (parseTeX "" $
         [ mkLetter 'a'
         , mkCtrlSeq "hello"
         , mkLetter 'z'
         ])
  , "unknown command with optional and mandatory arguments"
    ~: [Command "cmd"
        ([[Plain "opt1"], [Plain "opt2"]],
         [[Plain "man1"], [Plain "man2"]]),
        Plain "z"]
    ~=? (parseTeX "" $ mkCtrlSeq "cmd" :
          mkOptArg (mkString "opt" ++ [mkOther '1']) ++
          mkOptArg (mkString "opt" ++ [mkOther '2']) ++
          mkGroup (mkString "man" ++ [mkOther '1']) ++
          mkGroup (mkString "man" ++ [mkOther '2']) ++
          [mkLetter 'z'])
  , "known command which takes no arguments ignores following group"
    ~: [Command "rm" ([],[]), Group "" ([],[]) [Plain "no arg"]]
    ~=? (parseTeX "" $
         mkCtrlSeq "rm" :
         mkGroup (mkString "no arg"))
  , "known command which takes no arguments ignores following optgroup"
    ~: [Command "rm" ([],[]), Plain "[no arg]"]
    ~=? (parseTeX "" $
         mkCtrlSeq "rm" :
         mkOptArg (mkString "no arg"))
  ]

-- collect all tests
tests :: Test
tests = TestList
  [ testsBasic
  ]

-- run tests
main :: IO ExitCode
main = do
  counts <- runTestTT tests
  if errors counts == 0 && failures counts == 0
    then exitSuccess
    else exitFailure
