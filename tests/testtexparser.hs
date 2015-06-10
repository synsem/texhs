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
import Text.TeX.Lexer.Token (Token(TeXChar,CtrlSeq), parTok)
import Text.TeX.Parser (parseTeX)
import Text.TeX.Parser.Types (TeXAtom(..), MathType(..))

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

mkEnv :: String -> [Token] -> [Token]
mkEnv name body =
  mkCtrlSeq "begin" : mkGroup (mkString name) ++ body ++
  (mkCtrlSeq "end" : mkGroup (mkString name))

between :: Token -> Token -> [Token] -> [Token]
between open close content = open : (content ++ [close])

spcTok :: Token
spcTok = TeXChar ' ' Space

eolTok :: Token
eolTok = TeXChar '\n' Eol

alignTok :: Token
alignTok = TeXChar '&' AlignTab

subTok :: Token
subTok = TeXChar '_' Subscript

supTok :: Token
supTok = TeXChar '^' Supscript

mathTok :: Token
mathTok = TeXChar '$' Mathshift

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
  , "subscripted single character"
    ~: [SubScript [Plain "a"], Plain "bc"]
    ~=? (parseTeX "" $ subTok : mkString "abc")
  , "subscripted group"
    ~: [SubScript [Plain "abc"], Plain "x"]
    ~=? (parseTeX "" $ subTok : mkGroup (mkString "abc") ++ mkString "x")
  , "superscripted single character"
    ~: [SupScript [Plain "a"], Plain "bc"]
    ~=? (parseTeX "" $ supTok : mkString "abc")
  , "superscripted group"
    ~: [SupScript [Plain "abc"], Plain "x"]
    ~=? (parseTeX "" $ supTok : mkGroup (mkString "abc") ++ mkString "x")
  , "two paragraphs"
    ~: [Plain "abc", Par, Plain "cba"]
    ~=? (parseTeX "" $ mkString "abc" ++ [parTok] ++ mkString "cba")
  , "two hard newlines"
    ~: [Plain "ab", Newline, Plain "cd", Newline, Plain "x"]
    ~=? (parseTeX "" $ mkString "ab" ++ [mkCtrlSeq "\\"] ++
         mkOptArg (mkOther '2' : mkString "cm") ++
         mkString "cd" ++ [mkCtrlSeq "\\"] ++ mkString "x")
  , "simple 2x2 tabular"
    ~: [Group "tabular" ([[]], [[Plain "ll"]])
        [Plain "a", AlignMark, Plain "b", Newline,
         Plain "c", AlignMark, Plain "d", Newline]]
    ~=? (parseTeX "" $ mkEnv "tabular" (mkGroup (mkString "ll") ++
         mkString "a" ++ [alignTok] ++ mkString "b" ++ [mkCtrlSeq "\\"] ++
         mkString "c" ++ [alignTok] ++ mkString "d" ++ [mkCtrlSeq "\\"]))
  , "nested environments"
    ~: [Group "a" ([],[])
        [Group "b" ([],[])
         [Group "c" ([],[]) []]]]
    ~=? (parseTeX "" $ mkEnv "a" (mkEnv "b" (mkEnv "c" [])))
  , "simple inline math"
    ~: [MathGroup MathInline [Plain "a"]]
    ~=? (parseTeX "" $ mathTok : mkLetter 'a' : [mathTok])
  , "simple display math"
    ~: [MathGroup MathDisplay [Plain "a"]]
    ~=? (parseTeX "" $ replicate 2 mathTok ++ mkString "a" ++
         replicate 2 mathTok)
  , "embedded math"
    ~: [MathGroup MathInline
        [ Command "text"
          ([], [[Plain "t", MathGroup MathDisplay [Plain "a"]]])
        , Plain "b"]]
    ~=? (parseTeX "" $ mathTok : mkCtrlSeq "text" :
         mkGroup (mkString "t" ++ replicate 2 mathTok ++
                 mkString "a" ++ replicate 2 mathTok) ++
         mkString "b" ++ [mathTok])
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
