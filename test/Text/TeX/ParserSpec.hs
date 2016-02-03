----------------------------------------------------------------------
--
-- Module      :  Text.TeX.ParserSpec
-- Copyright   :  2015-2016 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
-- Maintainer  :  mathias.schenner@langsci-press.org
--
-- Tests for the "Text.TeX.Parser" module.
----------------------------------------------------------------------

module Text.TeX.ParserSpec
  ( tests
  ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

import Text.TeX.Lexer.Token
import Text.TeX.Parser (parseTeX)
import Text.TeX.Parser.Basic
import Text.TeX.Parser.Core (runTeXParser)
import Text.TeX.Parser.Types (TeXAtom(..), Arg(..), MathType(..))

-------------------- tests

tests :: Test
tests = testGroup "Text.TeX.ParserSpec"
  [ testsBasic
  , testsArguments
  , testsControlSequences
  , testsActiveChars
  , testsWhitespace
  , testsSyntactic
  , testsSymbols
  , testsAccents
  , testsLigatures
  ]

testsBasic :: Test
testsBasic = testGroup "basic"
  [ testCase "two words" $
    parseTeX "" (mkString "hello" ++ [eolTok, spcTok] ++ mkString "world")
    @?=
    [Plain "hello", White, Plain "world"]
  , testCase "command with no arguments" $
    parseTeX "" [mkCtrlSeq "hello"]
    @?=
    [Command "hello" []]
  , testCase "command with no arguments between letters" $
    parseTeX ""
      [ mkLetter 'a'
      , mkCtrlSeq "hello"
      , mkLetter 'z']
    @?=
    [Plain "a", Command "hello" [], Plain "z"]
  , testCase "unknown command with optional and mandatory arguments" $
    parseTeX ""
      (mkCtrlSeq "cmd" :
       mkOptArg (mkString "opt" ++ [mkOther '1']) ++
       mkOptArg (mkString "opt" ++ [mkOther '2']) ++
       mkGroup (mkString "man" ++ [mkOther '1']) ++
       mkGroup (mkString "man" ++ [mkOther '2']) ++
       [mkLetter 'z'])
    @?=
    [Command "cmd"
      [ OptArg [Plain "opt1"]
      , OptArg [Plain "opt2"]
      , OblArg [Plain "man1"]
      , OblArg [Plain "man2"]
      ], Plain "z"]
  , testCase "known command which takes no arguments ignores following group" $
    parseTeX "" (mkCtrlSeq "rm" : mkGroup (mkDefault "no arg"))
    @?=
    [Command "rm" [], Group "" [] [Plain "no", White, Plain "arg"]]
  , testCase "known command which takes no arguments ignores following optgroup" $
    parseTeX "" (mkCtrlSeq "rm" : mkOptArg (mkDefault "no arg"))
    @?=
    [Command "rm" [], Plain "[no", White, Plain "arg]"]
  , testCase "subscripted single character" $
    parseTeX "" (subTok : mkString "abc")
    @?=
    [SubScript [Plain "a"], Plain "bc"]
  , testCase "subscripted group" $
    parseTeX "" (subTok : mkGroup (mkString "abc") ++ mkString "x")
    @?=
    [SubScript [Plain "abc"], Plain "x"]
  , testCase "superscripted single character" $
    parseTeX "" (supTok : mkString "abc")
    @?=
    [SupScript [Plain "a"], Plain "bc"]
  , testCase "superscripted group" $
    parseTeX "" (supTok : mkGroup (mkString "abc") ++ mkString "x")
    @?=
    [SupScript [Plain "abc"], Plain "x"]
  , testCase "two paragraphs" $
    parseTeX "" (mkString "abc" ++ [parTok] ++ mkString "cba")
    @?=
    [Plain "abc", Par, Plain "cba"]
  , testCase "two hard newlines" $
    parseTeX ""
      (mkString "ab" ++ [mkCtrlSeq "\\"] ++
       mkOptArg (mkOther '2' : mkString "cm") ++
       mkString "cd" ++ [mkCtrlSeq "\\"] ++ mkString "x")
    @?=
    [Plain "ab", Newline, Plain "cd", Newline, Plain "x"]
  , testCase "simple 2x2 tabular" $
    parseTeX ""
      (mkEnv "tabular"
       (mkGroup (mkString "ll") ++
        mkString "a" ++ [alignTok] ++ mkString "b" ++ [mkCtrlSeq "\\"] ++
        mkString "c" ++ [alignTok] ++ mkString "d" ++ [mkCtrlSeq "\\"]))
    @?=
    [ Group "tabular" [OblArg [Plain "ll"]]
      [ Plain "a", AlignMark, Plain "b", Newline
      , Plain "c", AlignMark, Plain "d", Newline]]
  , testCase "nested environments" $
    parseTeX "" (mkEnv "a" (mkEnv "b" (mkEnv "c" [])))
    @?=
    [Group "a" [] [Group "b" [] [Group "c" [] []]]]
  , testCase "simple inline math" $
    parseTeX "" (mathTok : mkLetter 'a' : [mathTok])
    @?=
    [MathGroup MathInline [Plain "a"]]
  , testCase "simple display math" $
    parseTeX ""
      (replicate 2 mathTok ++ mkString "a" ++
       replicate 2 mathTok)
    @?=
    [MathGroup MathDisplay [Plain "a"]]
  , testCase "embedded math" $
    parseTeX ""
      (mathTok : mkCtrlSeq "text" :
       mkGroup (mkString "t" ++ replicate 2 mathTok ++
                mkString "a" ++ replicate 2 mathTok) ++
       mkString "b" ++ [mathTok])
    @?=
    [ MathGroup MathInline
      [ Command "text"
        [ OblArg [Plain "t", MathGroup MathDisplay [Plain "a"]]]
      , Plain "b"]]
  ]

testsArguments :: Test
testsArguments = testGroup "arguments"
  [ testCase "optional argument" $
    runTeXParser optarg "" (mkDefault "[hello]")
    @?=
    Right (OptArg [Plain "hello"])
  , testCase "optional argument in parentheses" $
    runTeXParser optargParens "" (mkDefault "(hello)")
    @?=
    Right (OptArg [Plain "hello"])
  , testCase "parentheses argument with embedded parens" $
    runTeXParser optargParens "" (mkDefault "(a(b)c)")
    @?=
    Right (OptArg [ Plain "a", Plain "(", Plain "b"
                  , Plain ")", Plain "c"])
  , testCase "parentheses argument with embedded group" $
    runTeXParser optargParens "" (mkDefault "(a{)}b)")
    @?=
    Right (OptArg [Plain "a", Group "" [] [Plain ")"], Plain "b"])
  , testCase "unknown control sequence may take single star argument" $
    parseTeX "" (mkCtrlSeq "unknown" : mkOther '*' : mkString "trailer")
    @?=
    [Command "unknown" [StarArg], Plain "trailer"]
  , testCase "unknown control sequence cannot take more than one star argument" $
    parseTeX "" (mkCtrlSeq "unknown" : mkOther '*' : mkOther '*' : mkString "trailer")
    @?=
    [Command "unknown" [StarArg], Plain "*trailer"]
  , testCase "default star argument cannot follow optional argument" $
    parseTeX "" (mkCtrlSeq "unknown" : mkOptArg (mkString "opt") ++
      [mkOther '*'] ++ mkString "trailer")
    @?=
    [Command "unknown" [OptArg [Plain "opt"]], Plain "*trailer"]
  , testCase "unknown control sequence may take multiple optional args" $
    parseTeX "" (mkCtrlSeq "unknown" : mkOther '*' :
      mkOptArg (mkDefault "opt1") ++
      mkOptArg (mkDefault "opt2") ++ mkString "trailer")
    @?=
    [ Command "unknown" [StarArg, OptArg [Plain "opt1"], OptArg [Plain "opt2"]]
    , Plain "trailer"]
  ]

testsControlSequences :: Test
testsControlSequences = testGroup "control sequences"
  [ testCase "unknown control sequence with no args" $
    runTeXParser command "" [mkCtrlSeq "unknown"]
    @?=
    Right (Command "unknown" [])
  , testCase "starred 'chapter' with optional arg" $
    runTeXParser command "" (mkCtrlSeq "chapter" : mkOther '*' :
      mkOptArg (mkString "short") ++ mkGroup (mkString "title"))
    @?=
    Right (Command "chapter"
      [StarArg, OptArg [Plain "short"], OblArg [Plain "title"]])
  , testCase "unstarred 'chapter' without optional arg" $
    runTeXParser command "" (mkCtrlSeq "chapter" : mkGroup (mkString "title"))
    @?=
    Right (Command "chapter" [OblArg [Plain "title"]])
  , testCase "'section' cannot take two mandatory args" $
    parseTeX "" (mkCtrlSeq "section" : mkOther '*' :
      mkGroup (mkString "title") ++ mkGroup (mkString "trailer"))
    @?=
    [ Command "section" [StarArg, OblArg [Plain "title"]]
    , Group "" [] [Plain "trailer"]]
  , testCase "'section' needs mandatory argument" $
    parseTeX "" (mkCtrlSeq "section" : mkOther '*' :
      mkOptArg (mkString "opt") ++ mkString "abc")
    @?=
    [ Command "section" [StarArg, OptArg [Plain "opt"], OblArg [Plain "a"]]
    , Plain "bc"]
  ]

testsActiveChars :: Test
testsActiveChars = testGroup "active characters"
  [ testCase "handle non-breaking space" $
    parseTeX "" (mkString "ab" ++ [CtrlSeq "~" True] ++ mkString "cd")
    @?=
    [Plain "ab", Plain "\x00A0", Plain "cd"]
  , testCase "allow non-breaking space in named environments" $
    parseTeX "" (mkEnv "a" [CtrlSeq "~" True])
    @?=
    [Group "a" [] [Plain "\x00A0"]]
  ]

testsWhitespace :: Test
testsWhitespace = testGroup "whitespace"
  [ testCase "conflate inner whitespace" $
    parseTeX "" [mkLetter 'a', spcTok, eolTok, spcTok, mkLetter 'z']
    @?=
    [Plain "a", White, Plain "z"]
  , testCase "conflate outer whitespace but do not strip it" $
    parseTeX "" [eolTok, spcTok, spcTok]
    @?=
    [White]
  , testCase "strip whitespace around explicit line breaks" $
    parseTeX "" [spcTok, eolTok, spcTok, mkCtrlSeq "\\", spcTok, eolTok]
    @?=
    [Newline]
  , testCase "preserve par after explicit line breaks" $
    parseTeX "" [spcTok, mkCtrlSeq "\\", spcTok, eolTok, parTok, spcTok]
    @?=
    [Newline, Par]
  , testCase "conflate leading whitespace" $
    parseTeX "" [spcTok, eolTok, spcTok, mkLetter 'a']
    @?=
    [White, Plain "a"]
  , testCase "conflate leading whitespace around par" $
    parseTeX "" [spcTok, eolTok, parTok, spcTok, mkLetter 'a']
    @?=
    [Par, Plain "a"]
  , testCase "conflate trailing whitespace" $
    parseTeX "" [mkLetter 'a', spcTok, eolTok, spcTok]
    @?=
    [Plain "a", White]
  , testCase "conflate trailing whitespace around par" $
    parseTeX "" [mkLetter 'a', spcTok, eolTok, parTok, spcTok]
    @?=
    [Plain "a", Par]
  , testCase "conflate leading and trailing whitespace" $
    parseTeX "" [eolTok, spcTok, mkLetter 'a', spcTok, eolTok]
    @?=
    [White, Plain "a", White]
  , testCase "conflate whitespace within a group" $
    parseTeX "" (mkGroup
      [eolTok, spcTok, mkLetter 'a', spcTok, parTok, eolTok])
    @?=
    [Group "" [] [White, Plain "a", Par]]
  , testCase "conflate whitespace within nested groups" $
    parseTeX "" (mkGroup
      ([spcTok, eolTok] ++
       mkGroup (mkGroup
         [ eolTok, spcTok, mkLetter 'a'
         , spcTok, parTok, eolTok])))
    @?=
    [ Group "" []
      [ White
      , Group "" []
        [ Group "" []
          [ White, Plain "a", Par]]]]
  , testCase "conflate whitespace within command arguments" $
    parseTeX ""
      (mkCtrlSeq "cmd" :
       mkOptArg [eolTok, spcTok, mkLetter 'a', spcTok, parTok, eolTok] ++
       mkOptArg (mkGroup (mkGroup
        [eolTok, spcTok, mkLetter 'b', spcTok, parTok, eolTok])))
    @?=
    [ Command "cmd"
      [ OptArg [ White, Plain "a", Par]
      , OptArg [ Group "" []
                 [ Group "" []
                   [ White, Plain "b", Par]]]]]
  , testCase "interpret control space" $
    parseTeX "" [mkLetter 'a', CtrlSeq " " False, mkLetter 'b']
    @?=
    [Plain "a", White, Plain "b"]
  , testCase "interpret control newline" $
    parseTeX "" [mkLetter 'a', CtrlSeq "\n" False, mkLetter 'b']
    @?=
    [Plain "a", White, Plain "b"]
  ]

testsSyntactic :: Test
testsSyntactic = testGroup "syntactic commands"
  [ testCase "simple discretionary with empty pre-break and post-break" $
    parseTeX "" (concat
      [ [mkCtrlSeq "discretionary"]
      , mkGroup []
      , mkGroup []
      , mkGroup [mkLetter 'c']])
    @?=
    [Plain "c"]
  , testCase "discretionary takes three arguments" $
    parseTeX "" (concat
      [ [mkCtrlSeq "discretionary"]
      , mkGroup [mkLetter 'a']
      , mkGroup [mkLetter 'b']
      , mkGroup [mkLetter 'c']
      , mkGroup [mkLetter 'd']])
    @?=
    [Plain "c", Group "" [] [Plain "d"]]
  ]

testsSymbols :: Test
testsSymbols = testGroup "symbols"
  [ testCase "top-level dagger" $
    parseTeX "" [mkCtrlSeq "dagger"]
    @?=
    [Plain "\x2020"]
  , testCase "dagger in cmd arg" $
    parseTeX "" (mkCtrlSeq "emph" : mkGroup [mkCtrlSeq "dagger"])
    @?=
    [Command "emph" [OblArg [Plain "\x2020"]]]
  , testCase "dagger in nested group" $
    parseTeX "" (mkGroup (mkGroup (mkGroup [mkCtrlSeq "dagger"])))
    @?=
    [Group "" [] [Group "" [] [Group "" [] [Plain "\x2020"]]]]
  , testCase "dagger within word" $
    parseTeX "" (mkString "bet" ++ [mkCtrlSeq "dagger"] ++ mkString "er")
    @?=
    [Plain "bet", Plain "\x2020", Plain "er"]
  , testCase "interpret discretionary hyphen" $
    parseTeX "" (concat
      [ mkString "a"
      , [mkCtrlSeq "-"]
      , mkString "b"])
    @?=
    [Plain "a", Plain "\x00AD", Plain "b"]
  , testCase "ignore italic correction" $
    parseTeX "" (concat
      [ mkString "a"
      , [mkCtrlSeq "/"]
      , mkString "b"])
    @?=
    [Plain "a", Plain "", Plain "b"]
  ]

testsAccents :: Test
testsAccents = testGroup "accents"
  [ testCase "top-level diaeresis applied to single char" $
    parseTeX "" (mkCtrlSeq "\"": mkString "ab")
    @?=
    [Plain "a\x0308", Plain "b"]
  , testCase "top-level tilde applied to single char" $
    parseTeX "" (mkCtrlSeq "~": mkString "ab")
    @?=
    [Plain "a\x0303", Plain "b"]
  , testCase "diaeresis applied to multi-character group argument" $
    parseTeX "" (mkCtrlSeq "\"": mkGroup (mkString "ab"))
    @?=
    [Plain "a\x0308\&b"]
  , testCase "diaeresis applied to dotless i in singleton group argument" $
    parseTeX "" (mkCtrlSeq "\"": mkGroup [mkCtrlSeq "i"])
    @?=
    [Plain "\x0131\x0308"]
  , testCase "circumflex applied to normal i" $
    parseTeX "" [mkCtrlSeq "^", mkLetter 'i']
    @?=
    [Plain "i\x0302"]
  , testCase "circumflex applied to dotless i in singleton group argument" $
    parseTeX "" (mkCtrlSeq "^": mkGroup [mkCtrlSeq "i"])
    @?=
    [Plain "\x0131\x0302"]
  , testCase "obligatory argument of circumflex may be a command" $
    runTeXParser texParser "" [mkCtrlSeq "^", mkCtrlSeq "i"]
    @?=
    Right [Command "^" [OblArg [Command "i" []]]]
  , testCase "circumflex applied to dotless i" $
    parseTeX "" [mkCtrlSeq "^", mkCtrlSeq "i"]
    @?=
    [Plain "\x0131\x0302"]
  , testCase "double accent in text mode: diaeresis and circumflex" $
    parseTeX "" (mkCtrlSeq "\"": mkGroup (mkCtrlSeq "^" : mkString "a"))
    @?=
    [Plain "a\x0302\x0308"]
  , testCase "double accent in math mode: diaeresis and circumflex" $
    parseTeX ""
      (mathTok :
      mkCtrlSeq "ddot" :
      mkGroup (mkCtrlSeq "hat" : mkString "a") ++
      [mathTok])
    @?=
    [MathGroup MathInline [Plain "a\x0302\x0308"]]
  ]

testsLigatures :: Test
testsLigatures = testGroup "TeX ligatures"
  [ testCase "smart quotes" $
    parseTeX "" (mkDefault "a ``b`c c'dd''.")
    @?=
    [ Plain "a", White, Plain "\x201C\&b\x2018\&c", White
    , Plain "c\x2019\&dd\x201D."]
  , testCase "smart quotes separated by spaces" $
    parseTeX "" (mkDefault "` `` '' '")
    @?=
    [ Plain "\x2018", White, Plain "\x201C", White
    , Plain "\x201D", White, Plain "\x2019"]
  , testCase "en dash between letters" $
    parseTeX "" (mkDefault "a--b")
    @?=
    [ Plain "a\x2013\&b"]
  , testCase "em dash between letters" $
    parseTeX "" (mkDefault "a---b")
    @?=
    [ Plain "a\x2014\&b"]
  , testCase "Spanish ligatures" $
    parseTeX "" (mkDefault "?`a!`b")
    @?=
    [ Plain "\x00BF\&a\x00A1\&b"]
  , testCase "Spanish ligatures surrounded by smart quotes" $
    parseTeX "" (mkDefault "?``a`!```b")
    @?=
    [ Plain "\x00BF\&\x2018\&a\x2018\&\x00A1\&\x201C\&b"]
  , testCase "smart quotes around question mark" $
    parseTeX "" (mkDefault "`?'")
    @?=
    [ Plain "\x2018?\x2019"]
  , testCase "smart quotes around exclamation mark" $
    parseTeX "" (mkDefault "`!'")
    @?=
    [ Plain "\x2018!\x2019"]
  , testCase "intervening empty group prevents ligature" $
    parseTeX "" (mkDefault "-{}-")
    @?=
    [ Plain "-", Group "" [] [], Plain "-"]
  , testCase "group structure can prevent ligatures" $
    parseTeX "" (mkDefault "-{-}-")
    @?=
    [ Plain "-", Group "" [] [Plain "-"], Plain "-"]
  ]
