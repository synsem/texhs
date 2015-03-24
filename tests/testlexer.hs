----------------------------------------------------------------------
-- Tests for the @Text.TeX.Lexer@ module.
----------------------------------------------------------------------
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
-- Maintainer  :  mathias.schenner@langsci-press.org
----------------------------------------------------------------------

module Main where

import System.Exit (ExitCode, exitSuccess, exitFailure)
import Test.HUnit (Test(..), Counts(..), test, (~:), (~=?), runTestTT)

import Text.TeX.Lexer (parseTeX)
import Text.TeX.Lexer.Token (Token(TeXChar,CtrlSeq))
import Text.TeX.Lexer.Catcode (Catcode(..))


testsBasic :: Test
testsBasic = TestLabel "basic" $ test
  [ "no digits in control sequences by default"
    ~: [(CtrlSeq "a" False), (TeXChar '1' Other)]
    ~=? (parseTeX "" "\\a1")
  , "non-letter characters form control symbols"
    ~: [(CtrlSeq "@" False), (TeXChar 'a' Letter),
        (CtrlSeq "7" False), (TeXChar '3' Other)]
    ~=? (parseTeX "" "\\@a\\73")
  ]

testsMacros :: Test
testsMacros = TestLabel "macros" $ test
  [ "macro definitions disappear in the token stream"
    ~: []
    ~=? (parseTeX "" "\\def\\hello{invisible string}")
  , "nested macro call: expand user-defined macro recursively"
    ~: replicate 3 (TeXChar 'a' Letter)
    ~=? (parseTeX "" "\\def\\a{a}\\def\\b{\\a\\a\\a}\\b")
  , "nested macro definition"
    ~: [(TeXChar '(' Other), (TeXChar 'y' Letter),
        (TeXChar ':' Other), (TeXChar 'x' Letter),
        (TeXChar ')' Other)]
    ~=? (parseTeX "" "\\def\\a{\\def\\b##1}\\a#2{(#2:#1)}\\b{x}{y}")
  , "braces around arguments are stripped in macro calls"
    ~: [(TeXChar '8' Other)]
    ~=? (parseTeX "" "\\def\\a#1{#1}\\a{8}")
  , "braces around arguments are stripped in macro calls: leaking macro in 1st arg"
    ~: [(TeXChar 'y' Letter)]
    ~=? (parseTeX "" "\\def\\a#1#2{#1#2}\\a{\\def\\x{y}}{\\x}")
  , "macro definition in second argument leaks into first argument"
    ~: [(TeXChar 'y' Letter)]
    ~=? (parseTeX "" "\\def\\a#1#2{#2#1}\\a{\\x}{\\def\\x{y}}")
  , "inner braces around arguments are preserved in macro calls: non-leaking macro in 1st arg"
    ~: [(TeXChar '{' Bgroup), (TeXChar '}' Egroup), (CtrlSeq "x" False)]
    ~=? (parseTeX "" "\\def\\a#1#2{#1#2}\\a{{\\def\\x{y}}}{\\x}")
  , "macro call with leaking macro definition"
    ~: [(CtrlSeq "b" False), (TeXChar 'b' Letter), (TeXChar 'b' Letter)]
    ~=? (parseTeX "" "\\def\\a{\\def\\b{b}\\b}\\b\\a\\b")
  ]

testsCatcode :: Test
testsCatcode = TestLabel "catcode" $ test
  [ "global catcode change"
    ~: [(TeXChar '#' Letter)]
    ~=? (parseTeX "" "\\catcode 35=11#")
  , "catcode changes have block-local scope"
    ~: [(TeXChar '$' Mathshift), (TeXChar '{' Bgroup), (TeXChar '$' Other),
        (TeXChar '}' Egroup), (TeXChar '$' Mathshift)]
    ~=? (parseTeX "" "${\\catcode`$=12$}$")
  , "add new escape character"
    ~: [(CtrlSeq "hello" False), (CtrlSeq "world" False), (TeXChar '!' Other)]
    ~=? (parseTeX "" "\\catcode`@=0\\hello@world!")
  , "allow colon in macro names"
    ~: [(CtrlSeq "hello:world" False), (TeXChar '!' Other)]
    ~=? (parseTeX "" "\\catcode`:=11\\hello:world!")
  ]

testsCatcodeInMacro :: Test
testsCatcodeInMacro = TestLabel "catcodes in macro" $ test
  [ "catcode change in macro body has global effect after call"
    ~: [(TeXChar '#' Letter)]
    ~=? (parseTeX "" "\\def\\a{\\catcode35=11}\\a #")
  , "catcode change in macro body has block-local effect after call"
    ~: [(TeXChar '{' Bgroup), (TeXChar '$' Other),
        (TeXChar '}' Egroup), (TeXChar '$' Mathshift)]
    ~=? (parseTeX "" "\\def\\a{\\catcode`$=12}{\\a $}$")
  ]

testsActiveChars :: Test
testsActiveChars = TestLabel "active characters" $ test
  [ "make character active"
    ~: [(CtrlSeq "|" True), (TeXChar 'b' Letter), (CtrlSeq "|" True),
        (CtrlSeq "|" False), (TeXChar '!' Other)]
    ~=? (parseTeX "" "\\catcode`|=13|b|\\|!")
  , "define macro for active character"
    ~: replicate 3 (TeXChar 'a' Letter)
    ~=? (parseTeX "" "\\catcode`|=13\\def|{a}|||")
  ]

testsChar :: Test
testsChar = TestLabel "char command" $ test
  [ "insert percent sign by number"
    ~: replicate 2 (TeXChar '%' Other)
    ~=? (parseTeX "" "\\char37\\char `\\%")
  , "insert backslash by number"
    ~: replicate 2 (TeXChar '\\' Other)
    ~=? (parseTeX "" "\\char 92\\char`\\\\")
  ]

-- collect all tests
tests :: Test
tests = TestList
  [ testsBasic
  , testsMacros
  , testsCatcode
  , testsCatcodeInMacro
  , testsActiveChars
  , testsChar
  ]

-- run tests
main :: IO ExitCode
main = do
  counts <- runTestTT tests
  if errors counts == 0 && failures counts == 0
    then exitSuccess
    else exitFailure
