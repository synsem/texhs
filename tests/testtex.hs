----------------------------------------------------------------------
-- Tests for the @Text.TeX@ module.
----------------------------------------------------------------------
-- Copyright   :  2015-2016 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
-- Maintainer  :  mathias.schenner@langsci-press.org
----------------------------------------------------------------------

module Main where

import System.Exit (ExitCode, exitSuccess, exitFailure)
import Test.HUnit (Test(..), Counts(..), test, (~:), (~=?), runTestTT)

import Text.TeX (readTeX)
import Text.TeX.Parser.Types (TeXAtom(..), Arg(..), MathType(..))

-------------------- tests

testsBasic :: Test
testsBasic = TestLabel "basic" $ test
  [ "merge spaces"
    ~: [Plain "a", White, Plain "b"]
    ~=? (readTeX "" "a  \n  b")
  , "two words"
    ~: [Plain "hello", White, Plain "world"]
    ~=? (readTeX "" "hello\n  world")
  , "command with no arguments"
    ~: [Command "hello" []]
    ~=? (readTeX "" "\\hello")
  , "command with no arguments between letters"
    ~: [Plain "a", Command "hello" [], Plain "z"]
    ~=? (readTeX "" "a\\hello z")
  , "unknown command with optional and mandatory arguments"
    ~: [Command "cmd"
        [ OptArg [Plain "opt1"]
        , OptArg [Plain "opt2"]
        , OblArg [Plain "man1"]
        , OblArg [Plain "man2"]
        ], Plain "z"]
    ~=? (readTeX "" "\\cmd[opt1][opt2]{man1}{man2}z")
  , "known command which takes no arguments ignores following group"
    ~: [Command "rm" [], Group "" [] [Plain "no-arg"]]
    ~=? (readTeX "" "\\rm{no-arg}")
  , "known command which takes no arguments ignores following optgroup"
    ~: [Command "rm" [], Plain "[no-arg]"]
    ~=? (readTeX "" "\\rm[no-arg]")
  , "subscripted single character"
    ~: [SubScript [Plain "a"], Plain "bc"]
    ~=? (readTeX "" "_abc")
  , "subscripted group"
    ~: [SubScript [Plain "abc"], Plain "x"]
    ~=? (readTeX "" "_{abc}x")
  , "superscripted single character"
    ~: [SupScript [Plain "a"], Plain "bc"]
    ~=? (readTeX "" "^abc")
  , "superscripted group"
    ~: [SupScript [Plain "abc"], Plain "x"]
    ~=? (readTeX "" "^{abc}x")
  , "two paragraphs"
    ~: [Plain "abc", Par, Plain "cba"]
    ~=? (readTeX "" "abc\n  \n  cba")
  , "two hard newlines"
    ~: [Plain "ab", Newline, Plain "cd", Newline, Plain "x"]
    ~=? (readTeX "" "ab\\\\[2cm]cd\\\\x")
  , "simple 2x2 tabular"
    ~: [Group "tabular" [OblArg [Plain "ll"]]
        [Plain "a", AlignMark, Plain "b", Newline,
         Plain "c", AlignMark, Plain "d", Newline]]
    ~=? (readTeX "" "\\begin{tabular}{ll}a&b\\\\c&d\\\\\\end{tabular}")
  , "nested environments"
    ~: [Group "a" []
        [Group "b" []
         [Group "c" [] []]]]
    ~=? (readTeX "" $ "\\begin{a}\\begin{b}\\begin{c}" ++
         "\\end{c}\\end{b}\\end{a}")
  , "simple inline math"
    ~: [MathGroup MathInline [Plain "a"]]
    ~=? (readTeX "" "$a$")
  , "simple display math"
    ~: [MathGroup MathDisplay [Plain "a"]]
    ~=? (readTeX "" "$$a$$")
  , "embedded math"
    ~: [MathGroup MathInline
        [ Command "text"
          [OblArg [Plain "t", MathGroup MathDisplay [Plain "a"]]]
        , Plain "b"]]
    ~=? (readTeX "" "$\\text{t$$a$$}b$")
  , "simple inline math from latex syntax"
    ~: [MathGroup MathInline [Plain "a"]]
    ~=? (readTeX "" "\\(a\\)")
  , "simple display math from latex syntax"
    ~: [MathGroup MathDisplay [Plain "a"]]
    ~=? (readTeX "" "\\[a\\]")
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
