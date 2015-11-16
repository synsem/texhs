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

import Text.TeX.Lexer.Token
import Text.TeX.Parser (parseTeX)
import Text.TeX.Parser.Types (TeXAtom(..), Arg(..), MathType(..))


-------------------- tests

testsBasic :: Test
testsBasic = TestLabel "basic" $ test
  [ "two words"
    ~: [Plain "hello", White, Plain "world"]
    ~=? (parseTeX "" $
         mkString "hello" ++
         [eolTok, spcTok] ++
         mkString "world")
  , "command with no arguments"
    ~: [Command "hello" []]
    ~=? (parseTeX "" [mkCtrlSeq "hello"])
  , "command with no arguments between letters"
    ~: [Plain "a", Command "hello" [], Plain "z"]
    ~=? (parseTeX "" $
         [ mkLetter 'a'
         , mkCtrlSeq "hello"
         , mkLetter 'z'
         ])
  , "unknown command with optional and mandatory arguments"
    ~: [Command "cmd"
        [ OptArg [Plain "opt1"]
        , OptArg [Plain "opt2"]
        , OblArg [Plain "man1"]
        , OblArg [Plain "man2"]
        ], Plain "z"]
    ~=? (parseTeX "" $ mkCtrlSeq "cmd" :
          mkOptArg (mkString "opt" ++ [mkOther '1']) ++
          mkOptArg (mkString "opt" ++ [mkOther '2']) ++
          mkGroup (mkString "man" ++ [mkOther '1']) ++
          mkGroup (mkString "man" ++ [mkOther '2']) ++
          [mkLetter 'z'])
  , "known command which takes no arguments ignores following group"
    ~: [Command "rm" [], Group "" [] [Plain "no arg"]]
    ~=? (parseTeX "" $
         mkCtrlSeq "rm" :
         mkGroup (mkString "no arg"))
  , "known command which takes no arguments ignores following optgroup"
    ~: [Command "rm" [], Plain "[no arg]"]
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
    ~: [Group "tabular" [OblArg [Plain "ll"]]
        [Plain "a", AlignMark, Plain "b", Newline,
         Plain "c", AlignMark, Plain "d", Newline]]
    ~=? (parseTeX "" $ mkEnv "tabular" (mkGroup (mkString "ll") ++
         mkString "a" ++ [alignTok] ++ mkString "b" ++ [mkCtrlSeq "\\"] ++
         mkString "c" ++ [alignTok] ++ mkString "d" ++ [mkCtrlSeq "\\"]))
  , "nested environments"
    ~: [Group "a" []
        [Group "b" []
         [Group "c" [] []]]]
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
          [OblArg [Plain "t", MathGroup MathDisplay [Plain "a"]]]
        , Plain "b"]]
    ~=? (parseTeX "" $ mathTok : mkCtrlSeq "text" :
         mkGroup (mkString "t" ++ replicate 2 mathTok ++
                 mkString "a" ++ replicate 2 mathTok) ++
         mkString "b" ++ [mathTok])
  ]

testsWhitespace :: Test
testsWhitespace = TestLabel "whitespace" $ test
  [ "conflate inner whitespace"
    ~: [Plain "a", White, Plain "z"]
    ~=? (parseTeX "" [mkLetter 'a', spcTok, eolTok, spcTok, mkLetter 'z'])
  , "conflate outer whitespace but do not strip it"
    ~: [White]
    ~=? (parseTeX "" [eolTok, spcTok, spcTok])
  , "strip whitespace around explicit line breaks"
    ~: [Newline]
    ~=? (parseTeX "" [spcTok, eolTok, spcTok, mkCtrlSeq "\\", spcTok, eolTok])
  , "preserve par after explicit line breaks"
    ~: [Newline, Par]
    ~=? (parseTeX "" [spcTok, mkCtrlSeq "\\", spcTok, eolTok, parTok, spcTok])
  , "conflate leading whitespace"
    ~: [White, Plain "a"]
    ~=? (parseTeX "" [spcTok, eolTok, spcTok, mkLetter 'a'])
  , "conflate leading whitespace around par"
    ~: [Par, Plain "a"]
    ~=? (parseTeX "" [spcTok, eolTok, parTok, spcTok, mkLetter 'a'])
  , "conflate trailing whitespace"
    ~: [Plain "a", White]
    ~=? (parseTeX "" [mkLetter 'a', spcTok, eolTok, spcTok])
  , "conflate trailing whitespace around par"
    ~: [Plain "a", Par]
    ~=? (parseTeX "" [mkLetter 'a', spcTok, eolTok, parTok, spcTok])
  , "conflate leading and trailing whitespace"
    ~: [White, Plain "a", White]
    ~=? (parseTeX "" [eolTok, spcTok, mkLetter 'a', spcTok, eolTok])
  , "conflate whitespace within a group"
    ~: [Group "" [] [White, Plain "a", Par]]
    ~=? (parseTeX "" $ mkGroup
         [eolTok, spcTok, mkLetter 'a', spcTok, parTok, eolTok])
  , "conflate whitespace within nested groups"
    ~: [Group "" []
        [White, Group "" []
         [Group "" []
          [White, Plain "a", Par]]]]
    ~=? (parseTeX "" $ mkGroup ([spcTok, eolTok] ++ mkGroup (mkGroup
         [eolTok, spcTok, mkLetter 'a', spcTok, parTok, eolTok])))
  , "conflate whitespace within command arguments"
    ~: [Command "cmd"
        [OptArg [White, Plain "a", Par]
        ,OptArg [Group "" []
                 [Group "" []
                  [White, Plain "b", Par]]]]]
    ~=? (parseTeX "" $ mkCtrlSeq "cmd" :
         mkOptArg
           [eolTok, spcTok, mkLetter 'a', spcTok, parTok, eolTok] ++
         mkOptArg (mkGroup (mkGroup
           [eolTok, spcTok, mkLetter 'b', spcTok, parTok, eolTok])))
  ]

testsSymbols :: Test
testsSymbols = TestLabel "symbols" $ test
  [ "top-level dagger"
    ~: [Plain "\x2020"]
    ~=? (parseTeX "" [mkCtrlSeq "dagger"])
  , "dagger in cmd arg"
    ~: [Command "emph" [OblArg [Plain "\x2020"]]]
    ~=? (parseTeX "" (mkCtrlSeq "emph" : mkGroup [mkCtrlSeq "dagger"]))
  , "dagger in nested group"
    ~: [Group "" [] [Group "" [] [Group "" [] [Plain "\x2020"]]]]
    ~=? (parseTeX "" (mkGroup (mkGroup (mkGroup [mkCtrlSeq "dagger"]))))
  , "dagger within word"
    ~: [Plain "bet", Plain "\x2020", Plain "er"]
    ~=? (parseTeX "" (mkString "bet" ++ [mkCtrlSeq "dagger"] ++ mkString "er"))
  ]

testsAccents :: Test
testsAccents = TestLabel "accents" $ test
  [ "top-level diaeresis applied to single char"
    ~: [Plain "a\x0308", Plain "b"]
    ~=? (parseTeX "" (mkCtrlSeq "\"": mkString "ab"))
  , "diaeresis applied to multi-character group argument"
    ~: [Plain "a\x0308\&b"]
    ~=? (parseTeX "" (mkCtrlSeq "\"": mkGroup (mkString "ab")))
  , "diaeresis applied to dotless i in singleton group argument"
    ~: [Plain "\x0131\x0308"]
    ~=? (parseTeX "" (mkCtrlSeq "\"": mkGroup [mkCtrlSeq "i"]))
  , "double accent in text mode: diaeresis and circumflex"
    ~: [Plain "a\x0302\x0308"]
    ~=? (parseTeX "" (mkCtrlSeq "\"": mkGroup (mkCtrlSeq "^" : mkString "a")))
  , "double accent in math mode: diaeresis and circumflex"
    ~: [MathGroup MathInline [Plain "a\x0302\x0308"]]
    ~=? (parseTeX "" (mathTok: mkCtrlSeq "ddot":
                      mkGroup (mkCtrlSeq "hat" : mkString "a") ++
                      [mathTok]))
  ]

-- collect all tests
tests :: Test
tests = TestList
  [ testsBasic
  , testsWhitespace
  , testsSymbols
  , testsAccents
  ]

-- run tests
main :: IO ExitCode
main = do
  counts <- runTestTT tests
  if errors counts == 0 && failures counts == 0
    then exitSuccess
    else exitFailure
