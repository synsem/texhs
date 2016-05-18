----------------------------------------------------------------------
--
-- Module      :  Text.TeX.LexerSpec
-- Copyright   :  2015-2016 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
-- Maintainer  :  mathias.schenner@langsci-press.org
--
-- Tests for the "Text.TeX.Lexer" module.
----------------------------------------------------------------------

module Text.TeX.LexerSpec
  ( tests
  ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

import Text.TeX.Lexer (lexTeX)
import Text.TeX.Lexer.Token
import Text.TeX.Lexer.Catcode (Catcode(..))


-------------------- tests

tests :: Test
tests = testGroup "Text.TeX.LexerSpec"
  [ testsBasic
  , testsWhitespace
  , testsComments
  , testsGrouping
  , testsConditionals
  , testsMacrosLet
  , testsMacrosDef
  , testsMacrosXparse
  , testsMacrosLaTeX2e
  , testsMacroEnvsXparse
  , testsMacroEnvsLaTeX2e
  , testsCatcode
  , testsCatcodeInMacro
  , testsActiveChars
  , testsChar
  , testsNumber
  , testsMeaning
  , testsIncludes
  ]

testsBasic :: Test
testsBasic = testGroup "basic"
  [ testCase "no digits in control sequences by default" $
    lexTeX "" "\\a1"
    @?=
    [CtrlSeq "a" False, TeXChar '1' Other]
  , testCase "non-letter characters form control symbols" $
    lexTeX "" "\\@a\\73"
    @?=
    [CtrlSeq "@" False, TeXChar 'a' Letter
    ,CtrlSeq "7" False, TeXChar '3' Other]
  ]

testsWhitespace :: Test
testsWhitespace = testGroup "whitespace"
  [ testCase "drop whitespace after control words" $
    lexTeX "" "\\hello  w"
    @?=
    [CtrlSeq "hello" False, TeXChar 'w' Letter]
  , testCase "do not drop whitespace after control symbols" $
    lexTeX "" "\\% a"
    @?=
    [CtrlSeq "%" False, TeXChar ' ' Space, TeXChar 'a' Letter]
  , testCase "drop linebreak after control words" $
    lexTeX "" "\\def\\a#1{#1.}\\a \n  bc"
    @?=
    [TeXChar 'b' Letter, TeXChar '.' Other, TeXChar 'c' Letter]
  , testCase "do not drop par after control words" $
    lexTeX "" "\\def\\a#1{#1.}\\a \n \n  bc"
    @?=
    [CtrlSeq "par" False, TeXChar '.' Other, TeXChar 'b' Letter
    ,TeXChar 'c' Letter]
  , testCase "drop leading whitespace from every line" $
    lexTeX "" "     a \n    b "
    @?=
    [TeXChar 'a' Letter, TeXChar ' ' Space
    ,TeXChar 'b' Letter, TeXChar ' ' Space]
  , testCase "do not drop leading whitespace if linebreak is escaped" $
    lexTeX "" " a\\\n b"
    @?=
    [TeXChar 'a' Letter, CtrlSeq "\n" False, TeXChar ' ' Space
    ,TeXChar 'b' Letter]
  , testCase "detect linebreaks within a paragraph" $
    lexTeX "" "a\n  b%\n  c \nd"
    @?=
    [TeXChar 'a' Letter, TeXChar ' ' Space, TeXChar 'b' Letter
    ,TeXChar 'c' Letter, TeXChar ' ' Space, TeXChar 'd' Letter]
  , testCase "parse trailing empty lines as par" $
    lexTeX "" "a\n  \n\n  "
    @?=
    [TeXChar 'a' Letter, CtrlSeq "par" False]
  , testCase "parse single empty line as par" $
    lexTeX "" "a\n\nb"
    @?=
    [TeXChar 'a' Letter, CtrlSeq "par" False, TeXChar 'b' Letter]
  , testCase "parse two empty lines as par" $
    lexTeX "" "a\n\n\nb"
    @?=
    [TeXChar 'a' Letter, CtrlSeq "par" False, TeXChar 'b' Letter]
  , testCase "parse empty line with some whitespace as par" $
    lexTeX "" "a\n   \n   b"
    @?=
    [TeXChar 'a' Letter, CtrlSeq "par" False, TeXChar 'b' Letter]
  , testCase "parse two empty lines with some whitespace as par" $
    lexTeX "" "a\n \n   \n   b"
    @?=
    [TeXChar 'a' Letter, CtrlSeq "par" False, TeXChar 'b' Letter]
  , testCase "parse multiple empty lines with interrupting comments as a single par" $
    lexTeX "" "a\n \n % comment! \n  \n  b"
    @?=
    [TeXChar 'a' Letter, CtrlSeq "par" False, TeXChar 'b' Letter]
  , testCase "drop whitespace before par" $
    lexTeX "" "a \n \n \n  b"
    @?=
    [mkLetter 'a', parTok, mkLetter 'b']
  ]

testsComments :: Test
testsComments = testGroup "comments"
  [ testCase "remove comment including linebreak from stream" $
    lexTeX "" "\\hello% undefined macro\nw"
    @?=
    [CtrlSeq "hello" False, TeXChar 'w' Letter]
  , testCase "remove multi-line comments from stream" $
    lexTeX "" "  a% some comment \n   %\n%\n   %\n%hi\nb"
    @?=
    [TeXChar 'a' Letter, TeXChar 'b' Letter]
  , testCase "remove comment with special characters" $
    lexTeX "" "a% special chars: \\ } { $ & # ^ _ ~ %%\\!##\\\n b"
    @?=
    [TeXChar 'a' Letter, TeXChar 'b' Letter]
  , testCase "ignore comments when parsing macro arguments: single argument" $
    lexTeX "" "\\def\\ab#1{.#1.}\\ab % comment!\n  {a8}"
    @?=
    [TeXChar '.' Other, TeXChar 'a' Letter, TeXChar '8' Other
    ,TeXChar '.' Other]
  , testCase "ignore comments when parsing macro arguments: two arguments" $
    lexTeX "" "\\def\\ab#1#2{.#2:#1.}\\ab % comment!\n  {a8}% \n  hi"
    @?=
    [TeXChar '.' Other, TeXChar 'h' Letter, TeXChar ':' Other
    ,TeXChar 'a' Letter, TeXChar '8' Other, TeXChar '.' Other
    ,TeXChar 'i' Letter]
  , testCase "parse empty line after comment as par" $
    lexTeX "" "a% some comment\n  \n8"
    @?=
    [TeXChar 'a' Letter, CtrlSeq "par" False, TeXChar '8' Other]
  , testCase "drop leading whitespace from lines following a comment" $
    lexTeX "" "a% some comment\n  8"
    @?=
    [TeXChar 'a' Letter, TeXChar '8' Other]
  , testCase "drop comment in an empty group" $
    lexTeX "" "{% some comment\n}"
    @?=
    mkGroup []
  , testCase "drop multiple comments in an empty group" $
    lexTeX "" "{% comment 1\n   % comment 2\n}"
    @?=
    mkGroup []
  , testCase "drop comment on final line before eof" $
    lexTeX "" "end% some comment"
    @?=
    mkString "end"
  ]

testsGrouping :: Test
testsGrouping = testGroup "grouping"
  [ testCase "'bgroup' and 'egroup' control sequences induce grouping" $
    lexTeX "" "\\bgroup\\def\\ab{cd}\\ab\\egroup\\ab"
    @?=
    [TeXChar '{' Bgroup, TeXChar 'c' Letter, TeXChar 'd' Letter
    ,TeXChar '}' Egroup, CtrlSeq "ab" False]
  , testCase "'begingroup' and 'endgroup' control sequences induce grouping" $
    lexTeX "" "\\begingroup\\def\\ab{cd}\\ab\\endgroup\\ab"
    @?=
    [TeXChar '{' Bgroup, TeXChar 'c' Letter, TeXChar 'd' Letter
    ,TeXChar '}' Egroup, CtrlSeq "ab" False]
  , testCase "'begin' and 'end' control sequences induce grouping" $
    lexTeX "" "\\begin{z}\\def\\ab{cd}\\ab\\end{z}\\ab"
    @?=
    [CtrlSeq "begin" False, TeXChar '{' Bgroup, TeXChar 'z' Letter
    ,TeXChar '}' Egroup, TeXChar 'c' Letter, TeXChar 'd' Letter
    ,CtrlSeq "end" False, TeXChar '{' Bgroup, TeXChar 'z' Letter
    ,TeXChar '}' Egroup, CtrlSeq "ab" False]
  ]

testsConditionals :: Test
testsConditionals = testGroup "conditionals"
  [ testCase "'iftrue' expands first branch, no 'else' branch" $
    lexTeX "" "\\iftrue t\\fi ."
    @?=
    [TeXChar 't' Letter, TeXChar '.' Other]
  , testCase "'iftrue' expands first branch, ignoring 'else' branch" $
    lexTeX "" "\\iftrue t\\else f\\fi ."
    @?=
    [TeXChar 't' Letter, TeXChar '.' Other]
  , testCase "'iffalse' expands second branch, case without 'else' branch" $
    lexTeX "" "\\iffalse t\\fi ."
    @?=
    [TeXChar '.' Other]
  , testCase "'iffalse' expands second branch, case with 'else' branch" $
    lexTeX "" "\\iffalse t\\else f\\fi ."
    @?=
    [TeXChar 'f' Letter, TeXChar '.' Other]
  , testCase "a conditional can produce an incomplete group: left brace" $
    lexTeX ""
      ("\\def\\leftbrace{\\iftrue{\\else}\\fi}" ++
       "\\leftbrace}")
    @?=
    [TeXChar '{' Bgroup, TeXChar '}' Egroup]
  , testCase "a conditional can produce an incomplete group: right brace" $
    lexTeX ""
      ("\\def\\rightbrace{\\iffalse{\\else}\\fi}" ++
       "{\\rightbrace")
    @?=
    [TeXChar '{' Bgroup, TeXChar '}' Egroup]
  , testCase "ending a conditional via macro expansion, with 'else' branch" $
    lexTeX ""
      ("\\def\\elsepart{\\else false!\\fi}" ++
       "\\iftrue y\\elsepart")
    @?=
    [TeXChar 'y' Letter]
  , testCase "ending a conditional via macro expansion, without 'else' branch" $
    lexTeX ""
      ("\\def\\fipart{x\\fi}" ++
       "\\iftrue y\\fipart")
    @?=
    [TeXChar 'y' Letter, TeXChar 'x' Letter]
  , testCase "nested conditionals: T(-)(F(-)(-))" $
    lexTeX "" "\\iftrue a\\else z\\iffalse y\\else b\\fi\\fi"
    @?=
    [TeXChar 'a' Letter]
  , testCase "nested conditionals: T(F(-)(-))(-)" $
    lexTeX "" "\\iftrue a\\iffalse y\\else b\\fi\\else z\\fi"
    @?=
    [TeXChar 'a' Letter, TeXChar 'b' Letter]
  , testCase "nested conditionals: F(F(-)(-))(T(F(-)(-))(-))" $
    lexTeX ""
      ("{\\iffalse z\\iffalse y\\else x\\fi\\else a" ++
       "\\iftrue b\\iffalse w\\else c\\fi\\fi\\fi}")
    @?=
    [TeXChar '{' Bgroup, TeXChar 'a' Letter, TeXChar 'b' Letter
    ,TeXChar 'c' Letter, TeXChar '}' Egroup]
  , testCase "no macro expansion in dead branch" $
    lexTeX "" (concat
      ["\\def\\mkhi{\\def\\hi{hi}}"
      ,"\\iftrue\\else\\mkhi\\fi"
      ,"\\hi"])
    @?=
    [CtrlSeq "hi" False]
  , testCase "macro expansion in active branch" $
    lexTeX "" (concat
      ["\\def\\mkhi{\\def\\hi{hi}}"
      ,"\\iftrue\\mkhi\\else\\fi"
      ,"\\hi"])
    @?=
    [TeXChar 'h' Letter, TeXChar 'i' Letter]
  , testCase "conditionals look at meanings in dead branch: 'iftrue' alias" $
    lexTeX "" "\\let\\iif\\iftrue\\iftrue0\\else\\iif\\fi\\fi"
    @?=
    [TeXChar '0' Other]
  , testCase "conditionals look at meanings in dead branch: 'else' alias" $
    lexTeX "" "\\let\\ielse\\else\\iffalse0\\ielse1\\fi"
    @?=
    [TeXChar '1' Other]
  , testCase "no macro expansion in dead branch: unnoticed 'else' alias" $
    lexTeX "" "\\def\\ielse{\\else}\\iffalse0\\ielse1\\fi"
    @?=
    []
  ]

testsMacrosLet :: Test
testsMacrosLet = testGroup "meaning assignments with let"
  [ testCase "let assignments disappear in the token stream" $
    lexTeX "" "\\let\\hello=\\bye"
    @?=
    []
  , testCase "assign digit to a macro" $
    lexTeX "" "\\let\\one12\\meaning\\one"
    @?=
    TeXChar '2' Other : mkQuote "the character 1"
  , testCase "assign param char to a macro" $
    lexTeX "" "\\let\\myhash=#2"
    @?=
    [TeXChar '2' Other]
  , testCase "local scope" $
    lexTeX "" "{\\let\\ab=a\\ab}\\meaning\\ab"
    @?=
    ([TeXChar '{' Bgroup, TeXChar 'a' Letter, TeXChar '}' Egroup] ++
     mkQuote "undefined")
  , testCase "let assignments are by value (by meaning) not by name" $
    lexTeX "" "\\def\\a{a}\\let\\l\\a\\def\\a{b}\\l\\a"
    @?=
    [TeXChar 'a' Letter, TeXChar 'b' Letter]
  , testCase "let overwrites def" $
    lexTeX "" "\\def\\a{a}\\let\\a=b\\a"
    @?=
    [TeXChar 'b' Letter]
  , testCase "def overwrites let" $
    lexTeX "" "\\let\\a=b\\def\\a{a}\\a"
    @?=
    [TeXChar 'a' Letter]
  , testCase "let vs def" $
    lexTeX ""
      ("\\def\\a{a}\\def\\b{\\a}\\let\\c\\b\\c" ++
       "\\def\\a{b}\\c\\def\\b{c}\\c")
    @?=
    [TeXChar 'a' Letter, TeXChar 'b' Letter, TeXChar 'b' Letter]
  ]

testsMacrosDef :: Test
testsMacrosDef = testGroup "def macro definitions"
  [ testCase "macro definitions disappear in the token stream" $
    lexTeX "" "\\def\\hello{invisible string}"
    @?=
    []
  , testCase "local scope" $
    lexTeX "" "{\\def\\a{a}\\a}\\a"
    @?=
    [TeXChar '{' Bgroup, TeXChar 'a' Letter, TeXChar '}' Egroup
    ,CtrlSeq "a" False]
  , testCase "mandatory undelimited parameters skip optional leading space" $
    lexTeX "" "\\def\\a#1#2#3{(#1:#2:#3)}\\a a  b c "
    @?=
    [TeXChar '(' Other, TeXChar 'a' Letter, TeXChar ':' Other
    ,TeXChar 'b' Letter, TeXChar ':' Other, TeXChar 'c' Letter
    ,TeXChar ')' Other, TeXChar ' ' Space]
  , testCase "mandatory undelimited parameters skip optional leading space" $
    lexTeX "" "\\def\\a#1#2{(#1:#2)}\\a a  {dd}"
    @?=
    [TeXChar '(' Other, TeXChar 'a' Letter, TeXChar ':' Other
    ,TeXChar 'd' Letter, TeXChar 'd' Letter, TeXChar ')' Other]
  , testCase "delimited parameters accept empty arguments" $
    lexTeX "" "\\def\\a#1\\b{(#1)}\\a\\b"
    @?=
    [TeXChar '(' Other, TeXChar ')' Other]
  , testCase "nested macro call: expand user-defined macro recursively" $
    lexTeX "" "\\def\\a{a}\\def\\b{\\a\\a\\a}\\b"
    @?=
    replicate 3 (TeXChar 'a' Letter)
  , testCase "nested macro definition" $
    lexTeX "" "\\def\\a{\\def\\b##1}\\a#2{(#2:#1)}\\b{x}{y}"
    @?=
    [TeXChar '(' Other, TeXChar 'y' Letter
    ,TeXChar ':' Other, TeXChar 'x' Letter
    ,TeXChar ')' Other]
  , testCase "braces around arguments are stripped in macro calls" $
    lexTeX "" "\\def\\a#1{#1}\\a{8}"
    @?=
    [TeXChar '8' Other]
  , testCase "braces around arguments are stripped in macro calls: leaking macro in 1st arg" $
    lexTeX "" "\\def\\a#1#2{#1#2}\\a{\\def\\x{y}}{\\x}"
    @?=
    [TeXChar 'y' Letter]
  , testCase "macro definition in second argument leaks into first argument" $
    lexTeX "" "\\def\\a#1#2{#2#1}\\a{\\x}{\\def\\x{y}}"
    @?=
    [TeXChar 'y' Letter]
  , testCase "inner braces around arguments are preserved in macro calls: non-leaking macro in 1st arg" $
    lexTeX "" "\\def\\a#1#2{#1#2}\\a{{\\def\\x{y}}}{\\x}"
    @?=
    [TeXChar '{' Bgroup, TeXChar '}' Egroup, CtrlSeq "x" False]
  , testCase "macro call with leaking macro definition" $
    lexTeX "" "\\def\\a{\\def\\b{b}\\b}\\b\\a\\b"
    @?=
    [CtrlSeq "b" False, TeXChar 'b' Letter, TeXChar 'b' Letter]
  , testCase "macro bodies are tokenized at definition time" $
    lexTeX ""
      ("\\catcode`@=0\\def\\defhi{\\def@hi{hi}}\\catcode`\\@=12" ++
       "\\hi\\defhi\\hi@")
    @?=
    [CtrlSeq "hi" False, TeXChar 'h' Letter, TeXChar 'i' Letter
    ,TeXChar '@' Other]
  ]

testsMacrosXparse :: Test
testsMacrosXparse = testGroup "xparse macro definitions"
  [ testCase "macro definitions disappear in the token stream" $
    lexTeX "" "\\DeclareDocumentCommand\\hello{}{invisible}"
    @?=
    []
  , testCase "no arguments in argspec" $
    lexTeX ""
      ("\\DeclareDocumentCommand\\a{ }{A.}" ++
       "B\\a{b}")
    @?=
    [TeXChar 'B' Letter, TeXChar 'A' Letter, TeXChar '.' Other
    ,TeXChar '{' Bgroup, TeXChar 'b' Letter, TeXChar '}' Egroup]
  , testCase "one mandatory argument with 'm'" $
    lexTeX ""
      ("\\DeclareDocumentCommand{\\add}{m}{#1+#1}" ++
       "\\add4!")
    @?=
    [TeXChar '4' Other, TeXChar '+' Other, TeXChar '4' Other
    ,TeXChar '!' Other]
  , testCase "two mandatory arguments with 'm'" $
    lexTeX ""
      ("\\DeclareDocumentCommand\\pair{ m m }{(#2:#1)}" ++
       "\\pair{a}{b}e")
    @?=
    [TeXChar '(' Other, TeXChar 'b' Letter, TeXChar ':' Other
    ,TeXChar 'a' Letter, TeXChar ')' Other, TeXChar 'e' Letter]
  , testCase "ignore '+' (long) flags" $
    lexTeX ""
      ("\\DeclareDocumentCommand\\a{ +m+m +m}{#2:#1}" ++
       "\\a{b}{c}{d}")
    @?=
    [TeXChar 'c' Letter, TeXChar ':' Other, TeXChar 'b' Letter]
  , testCase "required delimited argument with 'r'" $
    lexTeX ""
      ("\\DeclareDocumentCommand{\\a}{r.:m}{(#2|#1)}" ++
       "\\a.9:8\\a7")
    @?=
    [TeXChar '(' Other, TeXChar '8' Other, TeXChar '|' Other
    ,TeXChar '9' Other, TeXChar ')' Other, TeXChar '(' Other
    ,TeXChar '7' Other, TeXChar '|' Other, CtrlSeq "NoValue" False
    ,TeXChar ')' Other]
  , testCase "required delimited argument with 'R'" $
    lexTeX ""
      ("\\DeclareDocumentCommand{\\a}{R(){76}m}{[#2:#1]}" ++
       "\\a(9)8\\a54")
    @?=
    [TeXChar '[' Other, TeXChar '8' Other, TeXChar ':' Other
    ,TeXChar '9' Other, TeXChar ']' Other, TeXChar '[' Other
    ,TeXChar '5' Other, TeXChar ':' Other, TeXChar '7' Other
    ,TeXChar '6' Other, TeXChar ']' Other, TeXChar '4' Other]
  , testCase "optional argument with 'o'" $
    lexTeX ""
      ("\\DeclareDocumentCommand{\\a}{o}{(#1)}" ++
       "\\a[b].\\a")
    @?=
    [TeXChar '(' Other, TeXChar 'b' Letter, TeXChar ')' Other
    ,TeXChar '.' Other, TeXChar '(' Other, CtrlSeq "NoValue" False
    ,TeXChar ')' Other]
  , testCase "optional argument with 'O'" $
    lexTeX ""
      ("\\DeclareDocumentCommand{\\a}{O{z}}{(#1)}" ++
       "\\a\\a[b].")
    @?=
    [TeXChar '(' Other, TeXChar 'z' Letter, TeXChar ')' Other
    ,TeXChar '(' Other, TeXChar 'b' Letter, TeXChar ')' Other
    ,TeXChar '.' Other]
  , testCase "optional argument with 'd'" $
    lexTeX ""
      ("\\DeclareDocumentCommand{\\a}{d?!}{(#1)}" ++
       "\\a?bc!.\\a")
    @?=
    [TeXChar '(' Other, TeXChar 'b' Letter, TeXChar 'c' Letter
    ,TeXChar ')' Other, TeXChar '.' Other, TeXChar '(' Other
    ,CtrlSeq "NoValue" False, TeXChar ')' Other]
  , testCase "optional argument with 'D'" $
    lexTeX ""
      ("\\DeclareDocumentCommand{\\a}{D(){z}}{(#1)}" ++
       "\\a[]\\a(b).")
    @?=
    [TeXChar '(' Other, TeXChar 'z' Letter, TeXChar ')' Other
    ,TeXChar '[' Other, TeXChar ']' Other,  TeXChar '(' Other
    ,TeXChar 'b' Letter, TeXChar ')' Other, TeXChar '.' Other]
  , testCase "optional argument with 'g'" $
    lexTeX ""
      ("\\DeclareDocumentCommand{\\a}{g}{(#1)}" ++
       "\\a{bc}.")
    @?=
    [TeXChar '(' Other, TeXChar 'b' Letter, TeXChar 'c' Letter
    ,TeXChar ')' Other, TeXChar '.' Other]
  , testCase "optional argument with 'G'" $
    lexTeX ""
      ("\\DeclareDocumentCommand{\\a}{G{z}}{(#1)}" ++
       "\\a3\\a{bc}.")
    @?=
    [TeXChar '(' Other, TeXChar 'z' Letter, TeXChar ')' Other
    ,TeXChar '3' Other, TeXChar '(' Other, TeXChar 'b' Letter
    ,TeXChar 'c' Letter, TeXChar ')' Other, TeXChar '.' Other]
  , testCase "optional star with 's'" $
    lexTeX ""
      ("\\DeclareDocumentCommand{\\a}{sm}{(#2)}" ++
       "\\a*34.")
    @?=
    [TeXChar '(' Other, TeXChar '3' Other, TeXChar ')' Other
    ,TeXChar '4' Other, TeXChar '.' Other]
  , testCase "optional token with 't'" $
    lexTeX ""
      ("\\DeclareDocumentCommand{\\a}{t|m}{(#2)}" ++
       "\\a|34.")
    @?=
    [TeXChar '(' Other, TeXChar '3' Other, TeXChar ')' Other
    ,TeXChar '4' Other, TeXChar '.' Other]
  , testCase "required 'until' argument with 'u' (single token)" $
    lexTeX ""
      ("\\DeclareDocumentCommand{\\a}{u|}{(#1)}" ++
       "\\a hi|4.")
    @?=
    [TeXChar '(' Other, TeXChar 'h' Letter, TeXChar 'i' Letter
    ,TeXChar ')' Other, TeXChar '4' Other, TeXChar '.' Other]
  , testCase "required 'until' argument with 'u' (grouped)" $
    lexTeX ""
      ("\\DeclareDocumentCommand{\\a}{u{|k}}{(#1)}" ++
       "\\a hi|k4.")
    @?=
    [TeXChar '(' Other, TeXChar 'h' Letter, TeXChar 'i' Letter
    ,TeXChar ')' Other, TeXChar '4' Other, TeXChar '.' Other]
  , testCase "required 'u' and 'm' arguments" $
    lexTeX ""
      ("\\DeclareDocumentCommand{\\a}{u|m}{(#2:#1)}" ++
       "\\a hi|45.")
    @?=
    [TeXChar '(' Other, TeXChar '4' Other, TeXChar ':' Other
    ,TeXChar 'h' Letter, TeXChar 'i' Letter, TeXChar ')' Other
    ,TeXChar '5' Other, TeXChar '.' Other]
  , testCase "required 'until' argument with 'l'" $
    lexTeX ""
      ("\\DeclareDocumentCommand{\\a}{l}{(#1)}" ++
       "\\a3b{4}.")
    @?=
    [TeXChar '(' Other, TeXChar '3' Other, TeXChar 'b' Letter
    ,TeXChar ')' Other, TeXChar '{' Bgroup, TeXChar '4' Other
    ,TeXChar '}' Egroup, TeXChar '.' Other]
  , testCase "values of 's' arguments" $
    lexTeX ""
      ("\\DeclareDocumentCommand{\\a}{s}{#1}" ++
       "\\a*.\\a.*")
    @?=
    [CtrlSeq "BooleanTrue" False, TeXChar '.' Other
    ,CtrlSeq "BooleanFalse" False, TeXChar '.' Other
    ,TeXChar '*' Other]
  , testCase "values of 't' arguments" $
    lexTeX ""
      ("\\DeclareDocumentCommand{\\a}{t/}{#1}" ++
       "\\a/.\\a*/")
    @?=
    [CtrlSeq "BooleanTrue" False, TeXChar '.' Other
    ,CtrlSeq "BooleanFalse" False, TeXChar '*' Other
    ,TeXChar '/' Other]
  , testCase "test for presence of 'r' argument" $
    lexTeX ""
      ("\\DeclareDocumentCommand{\\a}{mr()}{\\IfNoValueTF{#2}{A}{B}}" ++
       "\\a{one}(two).\\a{one}.")
    @?=
    [TeXChar 'B' Letter, TeXChar '.' Other, TeXChar 'A' Letter
    ,TeXChar '.' Other]
  , testCase "test for presence of 'd' argument" $
    lexTeX ""
      ("\\DeclareDocumentCommand{\\a}{md()}{\\IfNoValueTF{#2}{A}{B}}" ++
       "\\a{one}(two).\\a{one}.")
    @?=
    [TeXChar 'B' Letter, TeXChar '.' Other, TeXChar 'A' Letter
    ,TeXChar '.' Other]
  , testCase "test for presence of 'o' argument" $
    lexTeX ""
      ("\\DeclareDocumentCommand{\\a}{mo}{\\IfNoValueTF{#2}{A}{B}}" ++
       "\\a{one}[two].\\a{one}.")
    @?=
    [TeXChar 'B' Letter, TeXChar '.' Other, TeXChar 'A' Letter
    ,TeXChar '.' Other]
  , testCase "test for presence of 's' argument" $
    lexTeX ""
      ("\\DeclareDocumentCommand{\\a}{s}{\\IfBooleanTF{#1}{A}{B}}" ++
       "\\a.*\\a*.")
    @?=
    [TeXChar 'B' Letter, TeXChar '.' Other, TeXChar '*' Other
    ,TeXChar 'A' Letter, TeXChar '.' Other]
  , testCase "test for presence of 's' argument, with less explicit grouping" $
    lexTeX ""
      ("\\DeclareDocumentCommand \\a {s} {\\IfBooleanTF #1 A  BC}" ++
       "\\a.*\\a*.")
    @?=
    [TeXChar 'B' Letter, TeXChar 'C' Letter, TeXChar '.' Other
    ,TeXChar '*' Other, TeXChar 'A' Letter, TeXChar 'C' Letter
    ,TeXChar '.' Other]
  , testCase "test for presence of 't' argument" $
    lexTeX ""
      ("\\DeclareDocumentCommand{\\a}{t/}{\\IfBooleanTF{#1}{A}{B}}" ++
       "\\a./\\a/.")
    @?=
    [TeXChar 'B' Letter, TeXChar '.' Other, TeXChar '/' Other
    ,TeXChar 'A' Letter, TeXChar '.' Other]
  , testCase "optional argument in call may contain closing delimiter if properly nested" $
    lexTeX ""
      ("\\DeclareDocumentCommand{\\a}{om}{(#1:#2)}" ++
       "\\a[\\b[bo]{bm}].")
    @?=
    [TeXChar '(' Other, CtrlSeq "b" False, TeXChar '[' Other
    ,TeXChar 'b' Letter, TeXChar 'o' Letter, TeXChar ']' Other
    ,TeXChar '{' Bgroup, TeXChar 'b' Letter, TeXChar 'm' Letter
    ,TeXChar '}' Egroup, TeXChar ':' Other, TeXChar '.' Other
    ,TeXChar ')' Other]
  , testCase "optional argument in call may contain closing delimiters if properly nested" $
    lexTeX ""
      ("\\DeclareDocumentCommand{\\a}{o}{(#1)}" ++
       "\\a[[[[!]]]].")
    @?=
    [TeXChar '(' Other, TeXChar '[' Other, TeXChar '[' Other
    ,TeXChar '[' Other, TeXChar '!' Other, TeXChar ']' Other
    ,TeXChar ']' Other, TeXChar ']' Other, TeXChar ')' Other
    ,TeXChar '.' Other]
  , testCase "optional argument in call may be unbalanced if in group" $
    lexTeX ""
      ("\\DeclareDocumentCommand{\\a}{o}{(#1)}" ++
       "\\a[{]]![[}].")
    @?=
    [TeXChar '(' Other, TeXChar '{' Bgroup, TeXChar ']' Other
    ,TeXChar ']' Other, TeXChar '!' Other,  TeXChar '[' Other
    ,TeXChar '[' Other, TeXChar '}' Egroup, TeXChar ')' Other
    ,TeXChar '.' Other]
  , testCase "global scope" $
    lexTeX ""
      ("\\bgroup{\\DeclareDocumentCommand{\\a}{mm}{#2.}}\\egroup" ++
       "\\a {one}24")
    @?=
    [TeXChar '{' Bgroup, TeXChar '{' Bgroup, TeXChar '}' Egroup
    ,TeXChar '}' Egroup, TeXChar '2' Other, TeXChar '.' Other
    ,TeXChar '4' Other]
  , testCase "redefine an existing macro" $
    lexTeX "" (concat
      ["\\NewDocumentCommand{\\a}{}{1}"
      ,"\\RenewDocumentCommand{\\a}{}{2}"
      ,"\\a"])
    @?=
    [TeXChar '2' Other]
  , testCase "declaring a macro will overwrite existing ones" $
    lexTeX "" (concat
      ["\\NewDocumentCommand{\\a}{}{1}"
      ,"\\DeclareDocumentCommand{\\a}{}{2}"
      ,"\\a"])
    @?=
    [TeXChar '2' Other]
  , testCase "providing a macro does not overwrite existing ones" $
    lexTeX "" (concat
      ["\\NewDocumentCommand{\\a}{}{1}"
      ,"\\ProvideDocumentCommand{\\a}{}{2}"
      ,"\\a"])
    @?=
    [TeXChar '1' Other]
  , testCase "provide a new macro" $
    lexTeX "" (concat
      ["\\ProvideDocumentCommand{\\a}{}{1}"
      ,"\\ProvideDocumentCommand{\\a}{}{2}"
      ,"\\a"])
    @?=
    [TeXChar '1' Other]
  , testCase "redefine a provided macro" $
    lexTeX "" (concat
      ["\\ProvideDocumentCommand{\\a}{}{1}"
      ,"\\RenewDocumentCommand{\\a}{}{2}"
      ,"\\a"])
    @?=
    [TeXChar '2' Other]
  , testCase "redefining a macro has global effects: renew in group" $
    lexTeX "" (concat
      ["\\DeclareDocumentCommand{\\a}{}{1}"
      ,"{\\RenewDocumentCommand{\\a}{}{2}}"
      ,"\\a"])
    @?=
    [TeXChar '{' Bgroup, TeXChar '}' Egroup, TeXChar '2' Other]
  , testCase "redefining a macro has global effects: renew in 'begingroup'/'endgroup'" $
    lexTeX "" (concat
      ["\\DeclareDocumentCommand{\\a}{}{1}"
      ,"\\begingroup\\RenewDocumentCommand{\\a}{}{2}\\endgroup"
      ,"\\a"])
    @?=
    [TeXChar '{' Bgroup, TeXChar '}' Egroup, TeXChar '2' Other]
  ]

testsMacrosLaTeX2e :: Test
testsMacrosLaTeX2e = testGroup "LaTeX2e macro definitions"
  [ testCase "macro definitions disappear in the token stream" $
    lexTeX "" "\\newcommand{\\a}{z}"
    @?=
    []
  , testCase "no arguments" $
    lexTeX "" "\\newcommand{\\a}{z}\\a"
    @?=
    [TeXChar 'z' Letter]
  , testCase "single mandatory argument" $
    lexTeX "" "\\newcommand{\\a}[1]{(#1)}\\a7"
    @?=
    [TeXChar '(' Other, TeXChar '7' Other, TeXChar ')' Other]
  , testCase "optional argument with empty default" $
    lexTeX "" "\\newcommand{\\a}[1][]{(#1)}\\a[7]\\a"
    @?=
    [TeXChar '(' Other, TeXChar '7' Other, TeXChar ')' Other
    ,TeXChar '(' Other, TeXChar ')' Other]
  , testCase "optional argument with non-empty default" $
    lexTeX "" "\\newcommand{\\a}[1][hi]{(#1)}\\a\\a[7]"
    @?=
    [TeXChar '(' Other, TeXChar 'h' Letter, TeXChar 'i' Letter
    ,TeXChar ')' Other, TeXChar '(' Other, TeXChar '7' Other
    ,TeXChar ')' Other]
  , testCase "mandatory and optional argument" $
    lexTeX ""
      ("\\newcommand{\\a}[2][z]{(#2:#1)}" ++
       "\\a{i}\\a[l]{o}\\a[]{}")
    @?=
    [TeXChar '(' Other, TeXChar 'i' Letter, TeXChar ':' Other
    ,TeXChar 'z' Letter, TeXChar ')' Other, TeXChar '(' Other
    ,TeXChar 'o' Letter, TeXChar ':' Other, TeXChar 'l' Letter
    ,TeXChar ')' Other,  TeXChar '(' Other, TeXChar ':' Other
    ,TeXChar ')' Other]
  , testCase "two mandatory arguments, starred" $
    lexTeX "" "\\newcommand*{\\a}[2]{(#2#1)}\\a{r}{xo}"
    @?=
    [TeXChar '(' Other, TeXChar 'x' Letter, TeXChar 'o' Letter
    ,TeXChar 'r' Letter, TeXChar ')' Other]
  , testCase "optional argument in call may contain closing bracket in group braces" $
    lexTeX "" "\\newcommand{\\a}[1][z]{(#1)}\\a[{]}]\\a"
    @?=
    [TeXChar '(' Other, TeXChar '{' Bgroup, TeXChar ']' Other
    ,TeXChar '}' Egroup, TeXChar ')' Other, TeXChar '(' Other
    ,TeXChar 'z' Letter, TeXChar ')' Other]
  , testCase "global scope" $
    lexTeX ""
      ("\\bgroup{\\newcommand{\\a}[2]{#2.}}\\egroup" ++
       "\\a {one}24")
    @?=
    [TeXChar '{' Bgroup, TeXChar '{' Bgroup, TeXChar '}' Egroup
    ,TeXChar '}' Egroup, TeXChar '2' Other, TeXChar '.' Other
    ,TeXChar '4' Other]
  , testCase "redefine an existing macro" $
    lexTeX "" (concat
      ["\\newcommand*{\\a}{1}"
      ,"\\renewcommand{\\a}{2}"
      ,"\\a"])
    @?=
    [TeXChar '2' Other]
  , testCase "declaring a macro will overwrite existing ones" $
    lexTeX "" (concat
      ["\\newcommand{\\a}{1}"
      ,"\\DeclareRobustCommand{\\a}{2}"
      ,"\\a"])
    @?=
    [TeXChar '2' Other]
  , testCase "providing a macro does not overwrite existing ones" $
    lexTeX "" (concat
      ["\\newcommand{\\a}{1}"
      ,"\\providecommand{\\a}{2}"
      ,"\\a"])
    @?=
    [TeXChar '1' Other]
  , testCase "provide a new macro" $
    lexTeX "" (concat
      ["\\providecommand{\\a}{1}"
      ,"\\providecommand{\\a}{2}"
      ,"\\a"])
    @?=
    [TeXChar '1' Other]
  , testCase "redefine a provided macro" $
    lexTeX "" (concat
      ["\\providecommand{\\a}{1}"
      ,"\\renewcommand{\\a}{2}"
      ,"\\a"])
    @?=
    [TeXChar '2' Other]
  , testCase "redefining a macro has global effects: renew in group" $
    lexTeX "" (concat
      ["\\newcommand{\\a}{1}"
      ,"{\\renewcommand{\\a}{2}}"
      ,"\\a"])
    @?=
    [TeXChar '{' Bgroup, TeXChar '}' Egroup, TeXChar '2' Other]
  ]

testsMacroEnvsXparse :: Test
testsMacroEnvsXparse = testGroup "xparse environment definitions"
  [ testCase "environment definitions disappear in the token stream" $
    lexTeX "" "\\DeclareDocumentEnvironment{hello}{}{start}{end}"
    @?=
    []
  , testCase "no arguments in argspec" $
    lexTeX ""
      ("\\DeclareDocumentEnvironment{a}{}{A}{C}" ++
       "\\begin{a}B\\end{a}")
    @?=
    [TeXChar 'A' Letter, TeXChar 'B' Letter, TeXChar 'C' Letter]
  , testCase "single mandatory argument" $
    lexTeX ""
      ("\\DeclareDocumentEnvironment{a}{m}{#1(}{)#1}" ++
       "\\begin{a}{9}A\\end{a}")
    @?=
    [TeXChar '9' Other, TeXChar '(' Other, TeXChar 'A' Letter
    ,TeXChar ')' Other, TeXChar '9' Other]
  , testCase "two mandatory arguments" $
    lexTeX ""
      ("\\DeclareDocumentEnvironment{a}{mm}{#2(}{)#1}" ++
       "\\begin{a}{\\one}{\\two}A\\end{a}")
    @?=
    [CtrlSeq "two" False, TeXChar '(' Other, TeXChar 'A' Letter
    ,TeXChar ')' Other, CtrlSeq "one" False]
  , testCase "one mandatory argument between two optional arguments ('omo')" $
    lexTeX ""
      ("\\DeclareDocumentEnvironment{a}{omo}{#3(}{)#2}" ++
       "\\begin{a}{\\one}[\\two]A\\end{a}")
    @?=
    [CtrlSeq "two" False, TeXChar '(' Other, TeXChar 'A' Letter
    ,TeXChar ')' Other, CtrlSeq "one" False]
  , testCase "redefine an existing environment" $
    lexTeX "" (concat
      ["\\NewDocumentEnvironment{a}{mm}{-}{|}"
      ,"\\RenewDocumentEnvironment{a}{}{(}{)}"
      ,"\\begin{a}B\\end{a}"])
    @?=
    [TeXChar '(' Other, TeXChar 'B' Letter, TeXChar ')' Other]
  , testCase "declaring an environment will overwrite existing ones" $
    lexTeX "" (concat
      ["\\NewDocumentEnvironment{a}{mm}{-}{|}"
      ,"\\DeclareDocumentEnvironment{a}{}{(}{)}"
      ,"\\begin{a}B\\end{a}"])
    @?=
    [TeXChar '(' Other, TeXChar 'B' Letter, TeXChar ')' Other]
  , testCase "endCode can access local definitions from startCode" $
    lexTeX ""
      ("\\DeclareDocumentEnvironment{a}{m}{\\def\\x{X}(}{)\\x:#1}" ++
       "\\begin{a}{9}A\\end{a}")
    @?=
    [TeXChar '(' Other, TeXChar 'A' Letter, TeXChar ')' Other
    ,TeXChar 'X' Letter, TeXChar ':' Other, TeXChar '9' Other]
  , testCase "body can access local definitions from startCode" $
    lexTeX ""
      ("\\DeclareDocumentEnvironment{a}{m}{\\def\\x{X}(}{)#1}" ++
       "\\begin{a}{9}[\\x]\\end{a}")
    @?=
    [TeXChar '(' Other, TeXChar '[' Other, TeXChar 'X' Letter
    ,TeXChar ']' Other, TeXChar ')' Other, TeXChar '9' Other]
  , testCase "startCode cannot access local definitions from endCode" $
    lexTeX ""
      ("\\DeclareDocumentEnvironment{a}{m}{\\x:#1(}{)\\def\\x{X}}" ++
       "\\begin{a}{9}A\\end{a}")
    @?=
    [CtrlSeq "x" False, TeXChar ':' Other, TeXChar '9' Other
    ,TeXChar '(' Other, TeXChar 'A' Letter, TeXChar ')' Other]
  , testCase "definitions in environments are group-local" $
    lexTeX ""
      ("\\DeclareDocumentEnvironment{a}{m}{\\def\\x{X}}{\\def\\z{Z}}" ++
       "\\begin{a}{9}\\def\\y{Y}\\end{a}\\x\\y\\z")
    @?=
    [CtrlSeq "x" False, CtrlSeq "y" False, CtrlSeq "z" False]
  ]

testsMacroEnvsLaTeX2e :: Test
testsMacroEnvsLaTeX2e = testGroup "LaTeX2e environment definitions"
  [ testCase "environment definitions disappear in the token stream" $
    lexTeX "" "\\newenvironment{hello}{start}{end}"
    @?=
    []
  , testCase "no arguments" $
    lexTeX ""
      ("\\newenvironment{a}{A}{C}" ++
       "\\begin{a}B\\end{a}")
    @?=
    [TeXChar 'A' Letter, TeXChar 'B' Letter, TeXChar 'C' Letter]
  , testCase "single mandatory argument" $
    lexTeX ""
      ("\\newenvironment{a}[1]{#1(}{)#1}" ++
       "\\begin{a}{9}A\\end{a}")
    @?=
    [TeXChar '9' Other, TeXChar '(' Other, TeXChar 'A' Letter
    ,TeXChar ')' Other, TeXChar '9' Other]
  , testCase "two mandatory arguments" $
    lexTeX ""
      ("\\newenvironment{a}[2]{#2(}{)#1}" ++
       "\\begin{a}{\\one}{\\two}A\\end{a}")
    @?=
    [CtrlSeq "two" False, TeXChar '(' Other, TeXChar 'A' Letter
    ,TeXChar ')' Other, CtrlSeq "one" False]
  , testCase "one optional and one mandatory argument" $
    lexTeX ""
      ("\\newenvironment{a}[2][default]{#2(}{)#1}" ++
       "\\begin{a}[\\one]{\\two}A\\end{a}")
    @?=
    [CtrlSeq "two" False, TeXChar '(' Other, TeXChar 'A' Letter
    ,TeXChar ')' Other, CtrlSeq "one" False]
  , testCase "redefine an existing environment" $
    lexTeX "" (concat
      ["\\newenvironment{a}[2]{-}{|}"
      ,"\\renewenvironment{a}{(}{)}"
      ,"\\begin{a}B\\end{a}"])
    @?=
    [TeXChar '(' Other, TeXChar 'B' Letter, TeXChar ')' Other]
  , testCase "allow subenvironments" $
    lexTeX ""
      ("\\newenvironment{abc}{\\begin{a}\\begin{b}\\begin{c}}" ++
       "{\\end{c}\\end{b}\\end{a}}" ++
       "\\begin{abc}d\\end{abc}")
    @?=
    mkEnv "a" (mkEnv "b" (mkEnv "c" [mkLetter 'd']))
  , testCase "allow subenvironments with arguments" $
    lexTeX ""
      ("\\newenvironment{ab}[1]{\\begin{a}#1\\begin{b}}" ++
       "{\\end{b}\\end{a}#1}" ++
       "\\begin{ab}cd\\end{ab}")
    @?=
    (mkEnv "a" (mkLetter 'c':mkEnv "b" [mkLetter 'd']) ++ [mkLetter 'c'])
  ]

testsCatcode :: Test
testsCatcode = testGroup "catcode"
  [ testCase "global catcode change" $
    lexTeX "" "\\catcode 35=11#"
    @?=
    [TeXChar '#' Letter]
  , testCase "catcode changes have block-local scope" $
    lexTeX "" "${\\catcode`$=12$}$"
    @?=
    [TeXChar '$' Mathshift, TeXChar '{' Bgroup, TeXChar '$' Other
    ,TeXChar '}' Egroup, TeXChar '$' Mathshift]
  , testCase "add new escape character" $
    lexTeX "" "\\catcode`@=0\\hello@world!"
    @?=
    [CtrlSeq "hello" False, CtrlSeq "world" False, TeXChar '!' Other]
  , testCase "add new block-local egroup character" $
    lexTeX "" "{{{\\catcode`)2))}}"
    @?=
    [TeXChar '{' Bgroup, TeXChar '{' Bgroup, TeXChar '{' Bgroup
    ,TeXChar ')' Egroup, TeXChar ')' Other, TeXChar '}' Egroup
    ,TeXChar '}' Egroup]
  , testCase "add new group delimiters" $
    lexTeX "" "{\\catcode`>=2\\catcode`<1a<}{>b>>"
    @?=
    [TeXChar '{' Bgroup, TeXChar 'a' Letter, TeXChar '<' Bgroup
    ,TeXChar '}' Egroup, TeXChar '{' Bgroup, TeXChar '>' Egroup
    ,TeXChar 'b' Letter, TeXChar '>' Egroup, TeXChar '>' Other]
  , testCase "allow colon in macro names" $
    lexTeX "" "\\catcode`:=11\\hello:world!"
    @?=
    [CtrlSeq "hello:world" False, TeXChar '!' Other]
  , testCase "copy catcode of another character" $
    lexTeX "" "\\catcode`%=\\catcode`8%"
    @?=
    [TeXChar '%' Other]
  ]

testsCatcodeInMacro :: Test
testsCatcodeInMacro = testGroup "catcodes in macro"
  [ testCase "catcode change in macro body has global effect after call" $
    lexTeX "" "\\def\\a{\\catcode35=11}\\a #"
    @?=
    [TeXChar '#' Letter]
  , testCase "catcode change in macro body has block-local effect after call" $
    lexTeX "" "\\def\\a{\\catcode`$=12}{\\a $}$"
    @?=
    [TeXChar '{' Bgroup, TeXChar '$' Other
    ,TeXChar '}' Egroup, TeXChar '$' Mathshift]
  ]

testsActiveChars :: Test
testsActiveChars = testGroup "active characters"
  [ testCase "make character active" $
    lexTeX "" "\\catcode`|=13|b|\\|!"
    @?=
    [CtrlSeq "|" True, TeXChar 'b' Letter, CtrlSeq "|" True
    ,CtrlSeq "|" False, TeXChar '!' Other]
  , testCase "define macro for active character" $
    lexTeX "" "\\catcode`|=13\\def|{a}|||"
    @?=
    replicate 3 (TeXChar 'a' Letter)
  , testCase "macro bodies may contain active characters" $
    lexTeX "" "\\def\\a#1{~#1~}\\a{b}"
    @?=
    [CtrlSeq "~" True, TeXChar 'b' Letter, CtrlSeq "~" True]
  ]

testsChar :: Test
testsChar = testGroup "char command"
  [ testCase "insert percent sign by number" $
    lexTeX "" "\\char37\\char `\\%"
    @?=
    replicate 2 (TeXChar '%' Other)
  , testCase "insert backslash by number" $
    lexTeX "" "\\char 92\\char`\\\\"
    @?=
    replicate 2 (TeXChar '\\' Other)
  ]

testsNumber :: Test
testsNumber = testGroup "number command"
  [ testCase "convert an internal hex number to dec tokens" $
    lexTeX "" "\\number\"2A"
    @?=
    [TeXChar '4' Other, TeXChar '2' Other]
  , testCase "print the character code of the percent sign" $
    lexTeX "" "\\number`\\%"
    @?=
    [TeXChar '3' Other, TeXChar '7' Other]
  , testCase "print the current catcode of the percent sign" $
    lexTeX "" "\\number\\catcode 37"
    @?=
    [TeXChar '1' Other, TeXChar '4' Other]
  , testCase "print the current catcode of the backslash character" $
    lexTeX "" "\\number\\catcode`\\\\"
    @?=
    [TeXChar '0' Other]
  ]

testsMeaning :: Test
testsMeaning = testGroup "meaning command"
  [ testCase "meaning of a letter" $
    lexTeX "" "\\meaning a"
    @?=
    mkQuote "the letter a"
  , testCase "meaning of a parameter character" $
    lexTeX "" "\\meaning#"
    @?=
    mkQuote "macro parameter character #"
  , testCase "meaning of a control sequence that denotes a primitive command" $
    lexTeX "" "\\meaning \\def"
    @?=
    mkQuote "primitive:def"
  , testCase "meaning of a user-defined macro" $
    lexTeX "" "\\def\\a#1{(#1)}\\meaning\\a"
    @?=
    mkQuote "macro:[Mandatory]->(#1)"
  , testCase "meaning of an undefined macro" $
    lexTeX "" "\\meaning\\a"
    @?=
    mkQuote "undefined"
  , testCase "meaning of the primitive \\undefined" $
    lexTeX "" "\\meaning\\undefined"
    @?=
    mkQuote "primitive:undefined"
  , testCase "the meaning of meaning" $
    lexTeX "" "\\meaning\\meaning"
    @?=
    mkQuote "primitive:meaning"
  ]

-- These tests run with 'lexTeX' over the identity monad, so @\\input@
-- and @\\include@ commands are ignored, but we can test the filename parser.
testsIncludes :: Test
testsIncludes = testGroup "include commands"
  [ testCase "input with whitespace-separated ascii filename" $
    lexTeX "" "\\input hello ab"
    @?=
    mkString "ab"
  , testCase "input with special characters in filename" $
    lexTeX "" "\\input z$&/#^_~.#!~ ab"
    @?=
    mkString "ab"
  , testCase "include with whitespace in group argument" $
    lexTeX "" "\\include{ hello }ab"
    @?=
    mkString "ab"
  , testCase "include without whitespace in group argument" $
    lexTeX "" "\\include{h$ll#}ab"
    @?=
    mkString "ab"
  ]
