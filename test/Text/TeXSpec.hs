----------------------------------------------------------------------
--
-- Module      :  Text.TeXSpec
-- Copyright   :  2015-2016 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
-- Maintainer  :  mathias.schenner@langsci-press.org
--
-- Tests for the "Text.TeX" module.
----------------------------------------------------------------------

module Text.TeXSpec
  ( tests
  ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

import Text.TeX (readTeX)
import Text.TeX.Parser.Types (TeXAtom(..), Arg(..), MathType(..))


-------------------- tests

tests :: Test
tests = testGroup "Text.TeXSpec"
  [ testsBasic
  ]

testsBasic :: Test
testsBasic = testGroup "basic"
  [ testCase "merge spaces" $
    readTeX "" "a  \n  b"
    @?=
    [Plain "a", White, Plain "b"]
  , testCase "two words" $
    readTeX "" "hello\n  world"
    @?=
    [Plain "hello", White, Plain "world"]
  , testCase "command with no arguments" $
    readTeX "" "\\hello"
    @?=
    [Command "hello" []]
  , testCase "command with no arguments between letters" $
    readTeX "" "a\\hello z"
    @?=
    [Plain "a", Command "hello" [], Plain "z"]
  , testCase "unknown command with optional and mandatory arguments" $
    readTeX "" "\\cmd[opt1][opt2]{man1}{man2}z"
    @?=
    [Command "cmd"
      [ OptArg [Plain "opt1"]
      , OptArg [Plain "opt2"]
      , OblArg [Plain "man1"]
      , OblArg [Plain "man2"]
      ], Plain "z"]
  , testCase "known command which takes no arguments ignores following group" $
    readTeX "" "\\rm{no-arg}"
    @?=
    [Command "rm" [], Group "" [] [Plain "no-arg"]]
  , testCase "known command which takes no arguments ignores following optgroup" $
    readTeX "" "\\rm[no-arg]"
    @?=
    [Command "rm" [], Plain "[no-arg]"]
  , testCase "subscripted single character" $
    readTeX "" "_abc"
    @?=
    [SubScript [Plain "a"], Plain "bc"]
  , testCase "subscripted group" $
    readTeX "" "_{abc}x"
    @?=
    [SubScript [Plain "abc"], Plain "x"]
  , testCase "superscripted single character" $
    readTeX "" "^abc"
    @?=
    [SupScript [Plain "a"], Plain "bc"]
  , testCase "superscripted group" $
    readTeX "" "^{abc}x"
    @?=
    [SupScript [Plain "abc"], Plain "x"]
  , testCase "two paragraphs" $
    readTeX "" "abc\n  \n  cba"
    @?=
    [Plain "abc", Par, Plain "cba"]
  , testCase "two hard newlines" $
    readTeX "" "ab\\\\[2cm]cd\\\\x"
    @?=
    [Plain "ab", Newline, Plain "cd", Newline, Plain "x"]
  , testCase "simple 2x2 tabular" $
    readTeX "" "\\begin{tabular}{ll}a&b\\\\c&d\\\\\\end{tabular}"
    @?=
    [Group "tabular" [OblArg [Plain "ll"]]
      [Plain "a", AlignMark, Plain "b", Newline,
       Plain "c", AlignMark, Plain "d", Newline]]
  , testCase "nested environments" $
    readTeX ""
      ("\\begin{a}\\begin{b}\\begin{c}" ++
       "\\end{c}\\end{b}\\end{a}")
    @?=
    [Group "a" [] [Group "b" [] [Group "c" [] []]]]
  , testCase "simple inline math" $
    readTeX "" "$a$"
    @?=
    [MathGroup MathInline [Plain "a"]]
  , testCase "simple display math" $
    readTeX "" "$$a$$"
    @?=
    [MathGroup MathDisplay [Plain "a"]]
  , testCase "embedded math" $
    readTeX "" "$\\text{t$$a$$}b$"
    @?=
    [ MathGroup MathInline
      [ Command "text"
        [ OblArg [Plain "t", MathGroup MathDisplay [Plain "a"]]]
      , Plain "b"]]
  , testCase "simple inline math from latex syntax" $
    readTeX "" "\\(a\\)"
    @?=
    [MathGroup MathInline [Plain "a"]]
  , testCase "simple display math from latex syntax" $
    readTeX "" "\\[a\\]"
    @?=
    [MathGroup MathDisplay [Plain "a"]]
  ]
