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

import Text.TeX.Lexer (lexTeX)
import Text.TeX.Lexer.Token
import Text.TeX.Lexer.Catcode (Catcode(..))


testsBasic :: Test
testsBasic = TestLabel "basic" $ test
  [ "no digits in control sequences by default"
    ~: [(CtrlSeq "a" False), (TeXChar '1' Other)]
    ~=? (lexTeX "" "\\a1")
  , "non-letter characters form control symbols"
    ~: [(CtrlSeq "@" False), (TeXChar 'a' Letter),
        (CtrlSeq "7" False), (TeXChar '3' Other)]
    ~=? (lexTeX "" "\\@a\\73")
  ]

testsWhitespace :: Test
testsWhitespace = TestLabel "whitespace" $ test
  [ "drop whitespace after control words"
    ~: [(CtrlSeq "hello" False), (TeXChar 'w' Letter)]
    ~=? (lexTeX "" "\\hello  w")
  , "do not drop whitespace after control symbols"
    ~: [(CtrlSeq "%" False), (TeXChar ' ' Space), (TeXChar 'a' Letter)]
    ~=? (lexTeX "" "\\% a")
  , "drop linebreak after control words"
    ~: [(TeXChar 'b' Letter), (TeXChar '.' Other), (TeXChar 'c' Letter)]
    ~=? (lexTeX "" "\\def\\a#1{#1.}\\a \n  bc")
  , "do not drop par after control words"
    ~: [(CtrlSeq "par" False), (TeXChar '.' Other), (TeXChar 'b' Letter),
        (TeXChar 'c' Letter)]
    ~=? (lexTeX "" "\\def\\a#1{#1.}\\a \n \n  bc")
  , "drop leading whitespace from every line"
    ~: [(TeXChar 'a' Letter), (TeXChar ' ' Space),
        (TeXChar 'b' Letter), (TeXChar ' ' Space)]
    ~=? (lexTeX "" "     a \n    b ")
  , "do not drop leading whitespace if linebreak is escaped"
    ~: [(TeXChar 'a' Letter), (CtrlSeq "\n" False), (TeXChar ' ' Space),
        (TeXChar 'b' Letter)]
    ~=? (lexTeX "" " a\\\n b")
  , "detect linebreaks within a paragraph"
    ~: [(TeXChar 'a' Letter), (TeXChar ' ' Space), (TeXChar 'b' Letter),
        (TeXChar 'c' Letter), (TeXChar ' ' Space), (TeXChar 'd' Letter)]
    ~=? (lexTeX "" "a\n  b%\n  c \nd")
  , "parse trailing empty lines as par"
    ~: [(TeXChar 'a' Letter), (CtrlSeq "par" False)]
    ~=? (lexTeX "" "a\n  \n\n  ")
  , "parse single empty line as par"
    ~: [(TeXChar 'a' Letter), (CtrlSeq "par" False), (TeXChar 'b' Letter)]
    ~=? (lexTeX "" "a\n\nb")
  , "parse two empty lines as par"
    ~: [(TeXChar 'a' Letter), (CtrlSeq "par" False), (TeXChar 'b' Letter)]
    ~=? (lexTeX "" "a\n\n\nb")
  , "parse empty line with some whitespace as par"
    ~: [(TeXChar 'a' Letter), (CtrlSeq "par" False), (TeXChar 'b' Letter)]
    ~=? (lexTeX "" "a\n   \n   b")
  , "parse two empty lines with some whitespace as par"
    ~: [(TeXChar 'a' Letter), (CtrlSeq "par" False), (TeXChar 'b' Letter)]
    ~=? (lexTeX "" "a\n \n   \n   b")
  , "parse multiple empty lines with interrupting comments as a single par"
    ~: [(TeXChar 'a' Letter), (CtrlSeq "par" False), (TeXChar 'b' Letter)]
    ~=? (lexTeX "" "a\n \n % comment! \n  \n  b")
  , "drop whitespace before par"
    ~: [mkLetter 'a', parTok, mkLetter 'b']
    ~=? (lexTeX "" "a \n \n \n  b")
  ]

testsComments :: Test
testsComments = TestLabel "comments" $ test
  [ "remove comment including linebreak from stream"
    ~: [(CtrlSeq "hello" False), (TeXChar 'w' Letter)]
    ~=? (lexTeX "" "\\hello% undefined macro\nw")
  , "remove multi-line comments from stream"
    ~: [(TeXChar 'a' Letter), (TeXChar 'b' Letter)]
    ~=? (lexTeX "" "  a% some comment \n   %\n%\n   %\n%hi\nb")
  , "remove comment with special characters"
    ~: [(TeXChar 'a' Letter), (TeXChar 'b' Letter)]
    ~=? (lexTeX "" "a% special chars: \\ } { $ & # ^ _ ~ %%\\!##\\\n b")
  , "ignore comments when parsing macro arguments: single argument"
    ~: [(TeXChar '.' Other), (TeXChar 'a' Letter), (TeXChar '8' Other),
        (TeXChar '.' Other)]
    ~=? (lexTeX "" "\\def\\ab#1{.#1.}\\ab % comment!\n  {a8}")
  , "ignore comments when parsing macro arguments: two arguments"
    ~: [(TeXChar '.' Other), (TeXChar 'h' Letter), (TeXChar ':' Other),
        (TeXChar 'a' Letter), (TeXChar '8' Other), (TeXChar '.' Other),
        (TeXChar 'i' Letter)]
    ~=? (lexTeX "" "\\def\\ab#1#2{.#2:#1.}\\ab % comment!\n  {a8}% \n  hi")
  , "parse empty line after comment as par"
    ~: [(TeXChar 'a' Letter), (CtrlSeq "par" False), (TeXChar '8' Other)]
    ~=? (lexTeX "" "a% some comment\n  \n8")
  , "drop leading whitespace from lines following a comment"
    ~: [(TeXChar 'a' Letter), (TeXChar '8' Other)]
    ~=? (lexTeX "" "a% some comment\n  8")
  , "drop comment in an empty group"
    ~: mkGroup []
    ~=? (lexTeX "" "{% some comment\n}")
  , "drop multiple comments in an empty group"
    ~: mkGroup []
    ~=? (lexTeX "" "{% comment 1\n   % comment 2\n}")
  , "drop comment on final line before eof"
    ~: mkString "end"
    ~=? (lexTeX "" "end% some comment")
  ]

testsGrouping :: Test
testsGrouping = TestLabel "grouping" $ test
  [ "'bgroup' and 'egroup' control sequences induce grouping"
    ~: [(TeXChar '{' Bgroup), (TeXChar 'c' Letter), (TeXChar 'd' Letter),
        (TeXChar '}' Egroup), (CtrlSeq "ab" False)]
    ~=? (lexTeX "" "\\bgroup\\def\\ab{cd}\\ab\\egroup\\ab")
  , "'begingroup' and 'endgroup' control sequences induce grouping"
    ~: [(TeXChar '{' Bgroup), (TeXChar 'c' Letter), (TeXChar 'd' Letter),
        (TeXChar '}' Egroup), (CtrlSeq "ab" False)]
    ~=? (lexTeX "" "\\begingroup\\def\\ab{cd}\\ab\\endgroup\\ab")
  , "'begin' and 'end' control sequences induce grouping"
    ~: [(CtrlSeq "begin" False), (TeXChar '{' Bgroup), (TeXChar 'z' Letter),
        (TeXChar '}' Egroup), (TeXChar 'c' Letter), (TeXChar 'd' Letter),
        (CtrlSeq "end" False), (TeXChar '{' Bgroup), (TeXChar 'z' Letter),
        (TeXChar '}' Egroup), (CtrlSeq "ab" False)]
    ~=? (lexTeX "" "\\begin{z}\\def\\ab{cd}\\ab\\end{z}\\ab")
  ]

testsConditionals :: Test
testsConditionals = TestLabel "conditionals" $ test
  [ "'iftrue' expands first branch, no 'else' branch"
    ~: [(TeXChar 't' Letter), (TeXChar '.' Other)]
    ~=? (lexTeX "" "\\iftrue t\\fi .")
  , "'iftrue' expands first branch, ignoring 'else' branch"
    ~: [(TeXChar 't' Letter), (TeXChar '.' Other)]
    ~=? (lexTeX "" "\\iftrue t\\else f\\fi .")
  , "'iffalse' expands second branch, case without 'else' branch"
    ~: [(TeXChar '.' Other)]
    ~=? (lexTeX "" "\\iffalse t\\fi .")
  , "'iffalse' expands second branch, case with 'else' branch"
    ~: [(TeXChar 'f' Letter), (TeXChar '.' Other)]
    ~=? (lexTeX "" "\\iffalse t\\else f\\fi .")
  , "a conditional can produce an incomplete group: left brace"
    ~: [(TeXChar '{' Bgroup), (TeXChar '}' Egroup)]
    ~=? (lexTeX "" ("\\def\\leftbrace{\\iftrue{\\else}\\fi}"
                      ++ "\\leftbrace}"))
  , "a conditional can produce an incomplete group: right brace"
    ~: [(TeXChar '{' Bgroup), (TeXChar '}' Egroup)]
    ~=? (lexTeX "" ("\\def\\rightbrace{\\iffalse{\\else}\\fi}"
                      ++ "{\\rightbrace"))
  , "ending a conditional via macro expansion, with 'else' branch"
    ~: [(TeXChar 'y' Letter)]
    ~=? (lexTeX "" ("\\def\\elsepart{\\else false!\\fi}"
                      ++ "\\iftrue y\\elsepart"))
  , "ending a conditional via macro expansion, without 'else' branch"
    ~: [(TeXChar 'y' Letter), (TeXChar 'x' Letter)]
    ~=? (lexTeX "" ("\\def\\fipart{x\\fi}"
                      ++ "\\iftrue y\\fipart"))
  , "nested conditionals: T(-)(F(-)(-))"
    ~: [(TeXChar 'a' Letter)]
    ~=? (lexTeX "" ("\\iftrue a\\else z\\iffalse y\\else b\\fi\\fi"))
  , "nested conditionals: T(F(-)(-))(-)"
    ~: [(TeXChar 'a' Letter), (TeXChar 'b' Letter)]
    ~=? (lexTeX "" ("\\iftrue a\\iffalse y\\else b\\fi\\else z\\fi"))
  , "nested conditionals: F(F(-)(-))(T(F(-)(-))(-))"
    ~: [(TeXChar '{' Bgroup), (TeXChar 'a' Letter), (TeXChar 'b' Letter),
        (TeXChar 'c' Letter), (TeXChar '}' Egroup)]
    ~=? (lexTeX "" ("{\\iffalse z\\iffalse y\\else x\\fi\\else a"
                     ++ "\\iftrue b\\iffalse w\\else c\\fi\\fi\\fi}"))
  , "no macro expansion in dead branch"
    ~: [(CtrlSeq "hi" False)]
    ~=? (lexTeX "" ("\\def\\mkhi{\\def\\hi{hi}}"
                      ++ "\\iftrue\\else\\mkhi\\fi" ++ "\\hi"))
  , "macro expansion in active branch"
    ~: [(TeXChar 'h' Letter), (TeXChar 'i' Letter)]
    ~=? (lexTeX "" ("\\def\\mkhi{\\def\\hi{hi}}"
                      ++ "\\iftrue\\mkhi\\else\\fi" ++ "\\hi"))
  ]

testsMacrosLet :: Test
testsMacrosLet = TestLabel "meaning assignments with let" $ test
  [ "let assignments disappear in the token stream"
    ~: []
    ~=? (lexTeX "" "\\let\\hello=\\bye")
  , "assign digit to a macro"
    ~: TeXChar '2' Other : (mkQuote "the character 1")
    ~=? (lexTeX "" "\\let\\one12\\meaning\\one")
  , "assign param char to a macro"
    ~: [TeXChar '2' Other]
    ~=? (lexTeX "" "\\let\\myhash=#2")
  , "local scope"
    ~: [TeXChar '{' Bgroup, TeXChar 'a' Letter, TeXChar '}' Egroup] ++
       (mkQuote "undefined")
    ~=? (lexTeX "" "{\\let\\ab=a\\ab}\\meaning\\ab")
  , "let assignments are by value (by meaning) not by name"
    ~: [TeXChar 'a' Letter, TeXChar 'b' Letter]
    ~=? (lexTeX "" "\\def\\a{a}\\let\\l\\a\\def\\a{b}\\l\\a")
  , "let overwrites def"
    ~: [TeXChar 'b' Letter]
    ~=? (lexTeX "" "\\def\\a{a}\\let\\a=b\\a")
  , "def overwrites let"
    ~: [TeXChar 'a' Letter]
    ~=? (lexTeX "" "\\let\\a=b\\def\\a{a}\\a")
  , "let vs def"
    ~: [TeXChar 'a' Letter, TeXChar 'b' Letter, TeXChar 'b' Letter]
    ~=? (lexTeX "" ("\\def\\a{a}\\def\\b{\\a}\\let\\c\\b\\c"
                    ++ "\\def\\a{b}\\c\\def\\b{c}\\c"))
  ]

testsMacrosDef :: Test
testsMacrosDef = TestLabel "def macro definitions" $ test
  [ "macro definitions disappear in the token stream"
    ~: []
    ~=? (lexTeX "" "\\def\\hello{invisible string}")
  , "local scope"
    ~: [(TeXChar '{' Bgroup), (TeXChar 'a' Letter), (TeXChar '}' Egroup),
        (CtrlSeq "a" False)]
    ~=? (lexTeX "" "{\\def\\a{a}\\a}\\a")
  , "mandatory undelimited parameters skip optional leading space"
    ~: [(TeXChar '(' Other), (TeXChar 'a' Letter), (TeXChar ':' Other),
        (TeXChar 'b' Letter), (TeXChar ':' Other), (TeXChar 'c' Letter),
        (TeXChar ')' Other), (TeXChar ' ' Space)]
    ~=? (lexTeX "" "\\def\\a#1#2#3{(#1:#2:#3)}\\a a  b c ")
  , "mandatory undelimited parameters skip optional leading space"
    ~: [(TeXChar '(' Other), (TeXChar 'a' Letter), (TeXChar ':' Other),
        (TeXChar 'd' Letter), (TeXChar 'd' Letter), (TeXChar ')' Other)]
    ~=? (lexTeX "" "\\def\\a#1#2{(#1:#2)}\\a a  {dd}")
  , "delimited parameters accept empty arguments"
    ~: [(TeXChar '(' Other), (TeXChar ')' Other)]
    ~=? (lexTeX "" "\\def\\a#1\\b{(#1)}\\a\\b")
  , "nested macro call: expand user-defined macro recursively"
    ~: replicate 3 (TeXChar 'a' Letter)
    ~=? (lexTeX "" "\\def\\a{a}\\def\\b{\\a\\a\\a}\\b")
  , "nested macro definition"
    ~: [(TeXChar '(' Other), (TeXChar 'y' Letter),
        (TeXChar ':' Other), (TeXChar 'x' Letter),
        (TeXChar ')' Other)]
    ~=? (lexTeX "" "\\def\\a{\\def\\b##1}\\a#2{(#2:#1)}\\b{x}{y}")
  , "braces around arguments are stripped in macro calls"
    ~: [(TeXChar '8' Other)]
    ~=? (lexTeX "" "\\def\\a#1{#1}\\a{8}")
  , "braces around arguments are stripped in macro calls: leaking macro in 1st arg"
    ~: [(TeXChar 'y' Letter)]
    ~=? (lexTeX "" "\\def\\a#1#2{#1#2}\\a{\\def\\x{y}}{\\x}")
  , "macro definition in second argument leaks into first argument"
    ~: [(TeXChar 'y' Letter)]
    ~=? (lexTeX "" "\\def\\a#1#2{#2#1}\\a{\\x}{\\def\\x{y}}")
  , "inner braces around arguments are preserved in macro calls: non-leaking macro in 1st arg"
    ~: [(TeXChar '{' Bgroup), (TeXChar '}' Egroup), (CtrlSeq "x" False)]
    ~=? (lexTeX "" "\\def\\a#1#2{#1#2}\\a{{\\def\\x{y}}}{\\x}")
  , "macro call with leaking macro definition"
    ~: [(CtrlSeq "b" False), (TeXChar 'b' Letter), (TeXChar 'b' Letter)]
    ~=? (lexTeX "" "\\def\\a{\\def\\b{b}\\b}\\b\\a\\b")
  , "macro bodies are tokenized at definition time"
    ~: [(CtrlSeq "hi" False), (TeXChar 'h' Letter), (TeXChar 'i' Letter),
        (TeXChar '@' Other)]
    ~=? (lexTeX "" ("\\catcode`@=0\\def\\defhi{\\def@hi{hi}}\\catcode`\\@=12"
                      ++ "\\hi\\defhi\\hi@"))
  ]

testsMacrosXparse :: Test
testsMacrosXparse = TestLabel "xparse macro definitions" $ test
  [ "macro definitions disappear in the token stream"
    ~: []
    ~=? (lexTeX "" "\\DeclareDocumentCommand\\hello{}{invisible}")
  , "no arguments in argspec"
    ~: [(TeXChar 'B' Letter), (TeXChar 'A' Letter), (TeXChar '.' Other),
        (TeXChar '{' Bgroup), (TeXChar 'b' Letter), (TeXChar '}' Egroup)]
    ~=? (lexTeX "" ("\\DeclareDocumentCommand\\a{ }{A.}"
                      ++ "B\\a{b}"))
  , "one mandatory argument with 'm'"
    ~: [(TeXChar '4' Other), (TeXChar '+' Other), (TeXChar '4' Other),
        (TeXChar '!' Other)]
    ~=? (lexTeX "" ("\\DeclareDocumentCommand{\\add}{m}{#1+#1}"
                      ++ "\\add4!"))
  , "two mandatory arguments with 'm'"
    ~: [(TeXChar '(' Other), (TeXChar 'b' Letter), (TeXChar ':' Other),
        (TeXChar 'a' Letter), (TeXChar ')' Other), (TeXChar 'e' Letter)]
    ~=? (lexTeX "" ("\\DeclareDocumentCommand\\pair{ m m }{(#2:#1)}"
                      ++ "\\pair{a}{b}e"))
  , "ignore '+' (long) flags"
    ~: [(TeXChar 'c' Letter), (TeXChar ':' Other), (TeXChar 'b' Letter)]
    ~=? (lexTeX "" ("\\DeclareDocumentCommand\\a{ +m+m +m}{#2:#1}"
                      ++ "\\a{b}{c}{d}"))
  , "required delimited argument with 'r'"
    ~: [(TeXChar '(' Other), (TeXChar '8' Other), (TeXChar '|' Other),
        (TeXChar '9' Other), (TeXChar ')' Other), (TeXChar '(' Other),
        (TeXChar '7' Other), (TeXChar '|' Other), (CtrlSeq "NoValue" False),
        (TeXChar ')' Other)]
    ~=? (lexTeX "" ("\\DeclareDocumentCommand{\\a}{r.:m}{(#2|#1)}"
                      ++ "\\a.9:8\\a7"))
  , "required delimited argument with 'R'"
    ~: [(TeXChar '[' Other), (TeXChar '8' Other), (TeXChar ':' Other),
        (TeXChar '9' Other), (TeXChar ']' Other), (TeXChar '[' Other),
        (TeXChar '5' Other), (TeXChar ':' Other), (TeXChar '7' Other),
        (TeXChar '6' Other), (TeXChar ']' Other), (TeXChar '4' Other)]
    ~=? (lexTeX "" ("\\DeclareDocumentCommand{\\a}{R(){76}m}{[#2:#1]}"
                      ++ "\\a(9)8\\a54"))
  , "optional argument with 'o'"
    ~: [(TeXChar '(' Other), (TeXChar 'b' Letter), (TeXChar ')' Other),
        (TeXChar '.' Other), (TeXChar '(' Other), (CtrlSeq "NoValue" False),
        (TeXChar ')' Other)]
    ~=? (lexTeX "" ("\\DeclareDocumentCommand{\\a}{o}{(#1)}"
                      ++ "\\a[b].\\a"))
  , "optional argument with 'O'"
    ~: [(TeXChar '(' Other), (TeXChar 'z' Letter), (TeXChar ')' Other),
        (TeXChar '(' Other), (TeXChar 'b' Letter), (TeXChar ')' Other),
        (TeXChar '.' Other)]
    ~=? (lexTeX "" ("\\DeclareDocumentCommand{\\a}{O{z}}{(#1)}"
                      ++ "\\a\\a[b]."))
  , "optional argument with 'd'"
    ~: [(TeXChar '(' Other), (TeXChar 'b' Letter), (TeXChar 'c' Letter),
        (TeXChar ')' Other), (TeXChar '.' Other), (TeXChar '(' Other),
        (CtrlSeq "NoValue" False), (TeXChar ')' Other)]
    ~=? (lexTeX "" ("\\DeclareDocumentCommand{\\a}{d?!}{(#1)}"
                      ++ "\\a?bc!.\\a"))
  , "optional argument with 'D'"
    ~: [(TeXChar '(' Other), (TeXChar 'z' Letter), (TeXChar ')' Other),
        (TeXChar '[' Other), (TeXChar ']' Other),  (TeXChar '(' Other),
        (TeXChar 'b' Letter), (TeXChar ')' Other), (TeXChar '.' Other)]
    ~=? (lexTeX "" ("\\DeclareDocumentCommand{\\a}{D(){z}}{(#1)}"
                      ++ "\\a[]\\a(b)."))
  , "optional argument with 'g'"
    ~: [(TeXChar '(' Other), (TeXChar 'b' Letter), (TeXChar 'c' Letter),
        (TeXChar ')' Other), (TeXChar '.' Other)]
    ~=? (lexTeX "" ("\\DeclareDocumentCommand{\\a}{g}{(#1)}"
                      ++ "\\a{bc}."))
  , "optional argument with 'G'"
    ~: [(TeXChar '(' Other), (TeXChar 'z' Letter), (TeXChar ')' Other),
        (TeXChar '3' Other), (TeXChar '(' Other), (TeXChar 'b' Letter),
        (TeXChar 'c' Letter), (TeXChar ')' Other), (TeXChar '.' Other)]
    ~=? (lexTeX "" ("\\DeclareDocumentCommand{\\a}{G{z}}{(#1)}"
                      ++ "\\a3\\a{bc}."))
  , "optional star with 's'"
    ~: [(TeXChar '(' Other), (TeXChar '3' Other), (TeXChar ')' Other),
        (TeXChar '4' Other), (TeXChar '.' Other)]
    ~=? (lexTeX "" ("\\DeclareDocumentCommand{\\a}{sm}{(#2)}"
                      ++ "\\a*34."))
  , "optional token with 't'"
    ~: [(TeXChar '(' Other), (TeXChar '3' Other), (TeXChar ')' Other),
        (TeXChar '4' Other), (TeXChar '.' Other)]
    ~=? (lexTeX "" ("\\DeclareDocumentCommand{\\a}{t|m}{(#2)}"
                      ++ "\\a|34."))
  , "required 'until' argument with 'u' (single token)"
    ~: [(TeXChar '(' Other), (TeXChar 'h' Letter), (TeXChar 'i' Letter),
        (TeXChar ')' Other), (TeXChar '4' Other), (TeXChar '.' Other)]
    ~=? (lexTeX "" ("\\DeclareDocumentCommand{\\a}{u|}{(#1)}"
                      ++ "\\a hi|4."))
  , "required 'until' argument with 'u' (grouped)"
    ~: [(TeXChar '(' Other), (TeXChar 'h' Letter), (TeXChar 'i' Letter),
        (TeXChar ')' Other), (TeXChar '4' Other), (TeXChar '.' Other)]
    ~=? (lexTeX "" ("\\DeclareDocumentCommand{\\a}{u{|k}}{(#1)}"
                      ++ "\\a hi|k4."))
  , "required 'u' and 'm' arguments"
    ~: [(TeXChar '(' Other), (TeXChar '4' Other), (TeXChar ':' Other),
        (TeXChar 'h' Letter), (TeXChar 'i' Letter), (TeXChar ')' Other),
        (TeXChar '5' Other), (TeXChar '.' Other)]
    ~=? (lexTeX "" ("\\DeclareDocumentCommand{\\a}{u|m}{(#2:#1)}"
                      ++ "\\a hi|45."))
  , "required 'until' argument with 'l'"
    ~: [(TeXChar '(' Other), (TeXChar '3' Other), (TeXChar 'b' Letter),
        (TeXChar ')' Other), (TeXChar '{' Bgroup), (TeXChar '4' Other),
        (TeXChar '}' Egroup), (TeXChar '.' Other)]
    ~=? (lexTeX "" ("\\DeclareDocumentCommand{\\a}{l}{(#1)}"
                      ++ "\\a3b{4}."))
  , "values of 's' arguments"
    ~: [(CtrlSeq "BooleanTrue" False), (TeXChar '.' Other),
        (CtrlSeq "BooleanFalse" False), (TeXChar '.' Other),
        (TeXChar '*' Other)]
    ~=? (lexTeX "" ("\\DeclareDocumentCommand{\\a}{s}{#1}"
                      ++ "\\a*.\\a.*"))
  , "values of 't' arguments"
    ~: [(CtrlSeq "BooleanTrue" False), (TeXChar '.' Other),
        (CtrlSeq "BooleanFalse" False), (TeXChar '*' Other),
        (TeXChar '/' Other)]
    ~=? (lexTeX "" ("\\DeclareDocumentCommand{\\a}{t/}{#1}"
                      ++ "\\a/.\\a*/"))
  , "test for presence of 'r' argument"
    ~: [(TeXChar 'B' Letter), (TeXChar '.' Other), (TeXChar 'A' Letter),
        (TeXChar '.' Other)]
    ~=? (lexTeX "" ("\\DeclareDocumentCommand{\\a}{mr()}{\\IfNoValueTF{#2}{A}{B}}"
                      ++ "\\a{one}(two).\\a{one}."))
  , "test for presence of 'd' argument"
    ~: [(TeXChar 'B' Letter), (TeXChar '.' Other), (TeXChar 'A' Letter),
        (TeXChar '.' Other)]
    ~=? (lexTeX "" ("\\DeclareDocumentCommand{\\a}{md()}{\\IfNoValueTF{#2}{A}{B}}"
                      ++ "\\a{one}(two).\\a{one}."))
  , "test for presence of 'o' argument"
    ~: [(TeXChar 'B' Letter), (TeXChar '.' Other), (TeXChar 'A' Letter),
        (TeXChar '.' Other)]
    ~=? (lexTeX "" ("\\DeclareDocumentCommand{\\a}{mo}{\\IfNoValueTF{#2}{A}{B}}"
                      ++ "\\a{one}[two].\\a{one}."))
  , "test for presence of 's' argument"
    ~: [(TeXChar 'B' Letter), (TeXChar '.' Other), (TeXChar '*' Other),
        (TeXChar 'A' Letter), (TeXChar '.' Other)]
    ~=? (lexTeX "" ("\\DeclareDocumentCommand{\\a}{s}{\\IfBooleanTF{#1}{A}{B}}"
                      ++ "\\a.*\\a*."))
  , "test for presence of 's' argument, with less explicit grouping"
    ~: [(TeXChar 'B' Letter), (TeXChar 'C' Letter), (TeXChar '.' Other),
        (TeXChar '*' Other), (TeXChar 'A' Letter), (TeXChar 'C' Letter),
        (TeXChar '.' Other)]
    ~=? (lexTeX "" ("\\DeclareDocumentCommand \\a {s} {\\IfBooleanTF #1 A  BC}"
                      ++ "\\a.*\\a*."))
  , "test for presence of 't' argument"
    ~: [(TeXChar 'B' Letter), (TeXChar '.' Other), (TeXChar '/' Other),
        (TeXChar 'A' Letter), (TeXChar '.' Other)]
    ~=? (lexTeX "" ("\\DeclareDocumentCommand{\\a}{t/}{\\IfBooleanTF{#1}{A}{B}}"
                      ++ "\\a./\\a/."))
  , "optional argument in call may contain closing delimiter if properly nested"
    ~: [(TeXChar '(' Other), (CtrlSeq "b" False), (TeXChar '[' Other),
        (TeXChar 'b' Letter), (TeXChar 'o' Letter), (TeXChar ']' Other),
        (TeXChar '{' Bgroup),(TeXChar 'b' Letter), (TeXChar 'm' Letter),
        (TeXChar '}' Egroup), (TeXChar ':' Other), (TeXChar '.' Other),
        (TeXChar ')' Other)]
    ~=? (lexTeX "" ("\\DeclareDocumentCommand{\\a}{om}{(#1:#2)}"
                      ++ "\\a[\\b[bo]{bm}]."))
  , "optional argument in call may contain closing delimiters if properly nested"
    ~: [(TeXChar '(' Other), (TeXChar '[' Other), (TeXChar '[' Other),
        (TeXChar '[' Other), (TeXChar '!' Other), (TeXChar ']' Other),
        (TeXChar ']' Other), (TeXChar ']' Other), (TeXChar ')' Other),
        (TeXChar '.' Other)]
    ~=? (lexTeX "" ("\\DeclareDocumentCommand{\\a}{o}{(#1)}"
                      ++ "\\a[[[[!]]]]."))
  , "optional argument in call may be unbalanced if in group"
    ~: [(TeXChar '(' Other), (TeXChar '{' Bgroup), (TeXChar ']' Other),
        (TeXChar ']' Other), (TeXChar '!' Other),  (TeXChar '[' Other),
        (TeXChar '[' Other), (TeXChar '}' Egroup), (TeXChar ')' Other),
        (TeXChar '.' Other)]
    ~=? (lexTeX "" ("\\DeclareDocumentCommand{\\a}{o}{(#1)}"
                      ++ "\\a[{]]![[}]."))
  , "global scope"
    ~: [(TeXChar '{' Bgroup), (TeXChar '{' Bgroup), (TeXChar '}' Egroup),
        (TeXChar '}' Egroup), (TeXChar '2' Other), (TeXChar '.' Other),
        (TeXChar '4' Other)]
    ~=? (lexTeX "" ("\\bgroup{\\DeclareDocumentCommand{\\a}{mm}{#2.}}\\egroup"
                      ++ "\\a {one}24"))
  , "redefine an existing macro"
    ~: [(TeXChar '2' Other)]
    ~=? (lexTeX "" ("\\NewDocumentCommand{\\a}{}{1}"
                      ++ "\\RenewDocumentCommand{\\a}{}{2}" ++ "\\a"))
  , "declaring a macro will overwrite existing ones"
    ~: [(TeXChar '2' Other)]
    ~=? (lexTeX "" ("\\NewDocumentCommand{\\a}{}{1}"
                      ++ "\\DeclareDocumentCommand{\\a}{}{2}" ++ "\\a"))
  , "providing a macro does not overwrite existing ones"
    ~: [(TeXChar '1' Other)]
    ~=? (lexTeX "" ("\\NewDocumentCommand{\\a}{}{1}"
                      ++ "\\ProvideDocumentCommand{\\a}{}{2}" ++ "\\a"))
  , "provide a new macro"
    ~: [(TeXChar '1' Other)]
    ~=? (lexTeX "" ("\\ProvideDocumentCommand{\\a}{}{1}"
                      ++ "\\ProvideDocumentCommand{\\a}{}{2}" ++ "\\a"))
  , "redefine a provided macro"
    ~: [(TeXChar '2' Other)]
    ~=? (lexTeX "" ("\\ProvideDocumentCommand{\\a}{}{1}"
                      ++ "\\RenewDocumentCommand{\\a}{}{2}" ++ "\\a"))
  , "redefining a macro has global effects: renew in group"
    ~: [(TeXChar '{' Bgroup), (TeXChar '}' Egroup), (TeXChar '2' Other)]
    ~=? (lexTeX "" ("\\DeclareDocumentCommand{\\a}{}{1}"
                      ++ "{\\RenewDocumentCommand{\\a}{}{2}}" ++ "\\a"))
  , "redefining a macro has global effects: renew in 'begingroup'/'endgroup'"
    ~: [(TeXChar '{' Bgroup), (TeXChar '}' Egroup), (TeXChar '2' Other)]
    ~=? (lexTeX "" ("\\DeclareDocumentCommand{\\a}{}{1}"
                      ++ "\\begingroup\\RenewDocumentCommand{\\a}{}{2}\\endgroup"
                      ++ "\\a"))
  ]

testsMacrosLaTeX2e :: Test
testsMacrosLaTeX2e = TestLabel "LaTeX2e macro definitions" $ test
  [ "macro definitions disappear in the token stream"
    ~: []
    ~=? (lexTeX "" "\\newcommand{\\a}{z}")
  , "no arguments"
    ~: [(TeXChar 'z' Letter)]
    ~=? (lexTeX "" "\\newcommand{\\a}{z}\\a")
  , "single mandatory argument"
    ~: [(TeXChar '(' Other), (TeXChar '7' Other), (TeXChar ')' Other)]
    ~=? (lexTeX "" "\\newcommand{\\a}[1]{(#1)}\\a7")
  , "optional argument with empty default"
    ~: [(TeXChar '(' Other), (TeXChar '7' Other), (TeXChar ')' Other),
        (TeXChar '(' Other), (TeXChar ')' Other)]
    ~=? (lexTeX "" "\\newcommand{\\a}[1][]{(#1)}\\a[7]\\a")
  , "optional argument with non-empty default"
    ~: [(TeXChar '(' Other), (TeXChar 'h' Letter), (TeXChar 'i' Letter),
        (TeXChar ')' Other), (TeXChar '(' Other), (TeXChar '7' Other),
        (TeXChar ')' Other)]
    ~=? (lexTeX "" "\\newcommand{\\a}[1][hi]{(#1)}\\a\\a[7]")
  , "mandatory and optional argument"
    ~: [(TeXChar '(' Other), (TeXChar 'i' Letter), (TeXChar ':' Other),
        (TeXChar 'z' Letter), (TeXChar ')' Other), (TeXChar '(' Other),
        (TeXChar 'o' Letter), (TeXChar ':' Other), (TeXChar 'l' Letter),
        (TeXChar ')' Other),  (TeXChar '(' Other), (TeXChar ':' Other),
        (TeXChar ')' Other)]
    ~=? (lexTeX "" ("\\newcommand{\\a}[2][z]{(#2:#1)}"
                      ++ "\\a{i}\\a[l]{o}\\a[]{}"))
  , "two mandatory arguments, starred"
    ~: [(TeXChar '(' Other), (TeXChar 'x' Letter), (TeXChar 'o' Letter),
        (TeXChar 'r' Letter), (TeXChar ')' Other)]
    ~=? (lexTeX "" "\\newcommand*{\\a}[2]{(#2#1)}\\a{r}{xo}")
  , "optional argument in call may contain closing bracket in group braces"
    ~: [(TeXChar '(' Other), (TeXChar '{' Bgroup), (TeXChar ']' Other),
        (TeXChar '}' Egroup), (TeXChar ')' Other), (TeXChar '(' Other),
        (TeXChar 'z' Letter), (TeXChar ')' Other)]
    ~=? (lexTeX "" "\\newcommand{\\a}[1][z]{(#1)}\\a[{]}]\\a")
  , "global scope"
    ~: [(TeXChar '{' Bgroup), (TeXChar '{' Bgroup), (TeXChar '}' Egroup),
        (TeXChar '}' Egroup), (TeXChar '2' Other), (TeXChar '.' Other),
        (TeXChar '4' Other)]
    ~=? (lexTeX "" ("\\bgroup{\\newcommand{\\a}[2]{#2.}}\\egroup"
                      ++ "\\a {one}24"))
  , "redefine an existing macro"
    ~: [(TeXChar '2' Other)]
    ~=? (lexTeX "" ("\\newcommand*{\\a}{1}"
                      ++ "\\renewcommand{\\a}{2}" ++ "\\a"))
  , "declaring a macro will overwrite existing ones"
    ~: [(TeXChar '2' Other)]
    ~=? (lexTeX "" ("\\newcommand{\\a}{1}"
                      ++ "\\DeclareRobustCommand{\\a}{2}" ++ "\\a"))
  , "providing a macro does not overwrite existing ones"
    ~: [(TeXChar '1' Other)]
    ~=? (lexTeX "" ("\\newcommand{\\a}{1}"
                      ++ "\\providecommand{\\a}{2}" ++ "\\a"))
  , "provide a new macro"
    ~: [(TeXChar '1' Other)]
    ~=? (lexTeX "" ("\\providecommand{\\a}{1}"
                      ++ "\\providecommand{\\a}{2}" ++ "\\a"))
  , "redefine a provided macro"
    ~: [(TeXChar '2' Other)]
    ~=? (lexTeX "" ("\\providecommand{\\a}{1}"
                      ++ "\\renewcommand{\\a}{2}" ++ "\\a"))
  , "redefining a macro has global effects: renew in group"
    ~: [(TeXChar '{' Bgroup), (TeXChar '}' Egroup), (TeXChar '2' Other)]
    ~=? (lexTeX "" ("\\newcommand{\\a}{1}"
                      ++ "{\\renewcommand{\\a}{2}}" ++ "\\a"))
  ]

testsMacroEnvsXparse :: Test
testsMacroEnvsXparse = TestLabel "xparse environment definitions" $ test
  [ "environment definitions disappear in the token stream"
    ~: []
    ~=? (lexTeX "" "\\DeclareDocumentEnvironment{hello}{}{start}{end}")
  , "no arguments in argspec"
    ~: [(TeXChar 'A' Letter), (TeXChar 'B' Letter), (TeXChar 'C' Letter)]
    ~=? (lexTeX "" ("\\DeclareDocumentEnvironment{a}{}{A}{C}"
                      ++ "\\begin{a}B\\end{a}"))
  , "single mandatory argument"
    ~: [(TeXChar '9' Other), (TeXChar '(' Other), (TeXChar 'A' Letter),
        (TeXChar ')' Other), (TeXChar '9' Other)]
    ~=? (lexTeX "" ("\\DeclareDocumentEnvironment{a}{m}{#1(}{)#1}"
                      ++ "\\begin{a}{9}A\\end{a}"))
  , "two mandatory arguments"
    ~: [(CtrlSeq "two" False), (TeXChar '(' Other), (TeXChar 'A' Letter),
        (TeXChar ')' Other), (CtrlSeq "one" False)]
    ~=? (lexTeX "" ("\\DeclareDocumentEnvironment{a}{mm}{#2(}{)#1}"
                      ++ "\\begin{a}{\\one}{\\two}A\\end{a}"))
  , "one mandatory argument between two optional arguments ('omo')"
    ~: [(CtrlSeq "two" False), (TeXChar '(' Other), (TeXChar 'A' Letter),
        (TeXChar ')' Other), (CtrlSeq "one" False)]
    ~=? (lexTeX "" ("\\DeclareDocumentEnvironment{a}{omo}{#3(}{)#2}"
                      ++ "\\begin{a}{\\one}[\\two]A\\end{a}"))
  , "redefine an existing environment"
    ~: [(TeXChar '(' Other), (TeXChar 'B' Letter), (TeXChar ')' Other)]
    ~=? (lexTeX "" ("\\NewDocumentEnvironment{a}{mm}{-}{|}"
                      ++ "\\RenewDocumentEnvironment{a}{}{(}{)}"
                      ++ "\\begin{a}B\\end{a}"))
  , "declaring an environment will overwrite existing ones"
    ~: [(TeXChar '(' Other), (TeXChar 'B' Letter), (TeXChar ')' Other)]
    ~=? (lexTeX "" ("\\NewDocumentEnvironment{a}{mm}{-}{|}"
                      ++ "\\DeclareDocumentEnvironment{a}{}{(}{)}"
                      ++ "\\begin{a}B\\end{a}"))
  , "endCode can access local definitions from startCode"
    ~: [(TeXChar '(' Other), (TeXChar 'A' Letter), (TeXChar ')' Other),
        (TeXChar 'X' Letter), (TeXChar ':' Other), (TeXChar '9' Other)]
    ~=? (lexTeX "" ("\\DeclareDocumentEnvironment{a}{m}{\\def\\x{X}(}{)\\x:#1}"
                      ++ "\\begin{a}{9}A\\end{a}"))
  , "body can access local definitions from startCode"
    ~: [(TeXChar '(' Other), (TeXChar '[' Other), (TeXChar 'X' Letter),
        (TeXChar ']' Other), (TeXChar ')' Other), (TeXChar '9' Other)]
    ~=? (lexTeX "" ("\\DeclareDocumentEnvironment{a}{m}{\\def\\x{X}(}{)#1}"
                      ++ "\\begin{a}{9}[\\x]\\end{a}"))
  , "startCode cannot access local definitions from endCode"
    ~: [(CtrlSeq "x" False), (TeXChar ':' Other), (TeXChar '9' Other),
        (TeXChar '(' Other), (TeXChar 'A' Letter), (TeXChar ')' Other)]
    ~=? (lexTeX "" ("\\DeclareDocumentEnvironment{a}{m}{\\x:#1(}{)\\def\\x{X}}"
                      ++ "\\begin{a}{9}A\\end{a}"))
  , "definitions in environments are group-local"
    ~: [(CtrlSeq "x" False), (CtrlSeq "y" False), (CtrlSeq "z" False)]
    ~=? (lexTeX "" ("\\DeclareDocumentEnvironment{a}{m}{\\def\\x{X}}{\\def\\z{Z}}"
                      ++ "\\begin{a}{9}\\def\\y{Y}\\end{a}\\x\\y\\z"))
  ]

testsMacroEnvsLaTeX2e :: Test
testsMacroEnvsLaTeX2e = TestLabel "LaTeX2e environment definitions" $ test
  [ "environment definitions disappear in the token stream"
    ~: []
    ~=? (lexTeX "" "\\newenvironment{hello}{start}{end}")
  , "no arguments"
    ~: [(TeXChar 'A' Letter), (TeXChar 'B' Letter), (TeXChar 'C' Letter)]
    ~=? (lexTeX "" ("\\newenvironment{a}{A}{C}"
                      ++ "\\begin{a}B\\end{a}"))
  , "single mandatory argument"
    ~: [(TeXChar '9' Other), (TeXChar '(' Other), (TeXChar 'A' Letter),
        (TeXChar ')' Other), (TeXChar '9' Other)]
    ~=? (lexTeX "" ("\\newenvironment{a}[1]{#1(}{)#1}"
                      ++ "\\begin{a}{9}A\\end{a}"))
  , "two mandatory arguments"
    ~: [(CtrlSeq "two" False), (TeXChar '(' Other), (TeXChar 'A' Letter),
        (TeXChar ')' Other), (CtrlSeq "one" False)]
    ~=? (lexTeX "" ("\\newenvironment{a}[2]{#2(}{)#1}"
                      ++ "\\begin{a}{\\one}{\\two}A\\end{a}"))
  , "one optional and one mandatory argument"
    ~: [(CtrlSeq "two" False), (TeXChar '(' Other), (TeXChar 'A' Letter),
        (TeXChar ')' Other), (CtrlSeq "one" False)]
    ~=? (lexTeX "" ("\\newenvironment{a}[2][default]{#2(}{)#1}"
                      ++ "\\begin{a}[\\one]{\\two}A\\end{a}"))
  , "redefine an existing environment"
    ~: [(TeXChar '(' Other), (TeXChar 'B' Letter), (TeXChar ')' Other)]
    ~=? (lexTeX "" ("\\newenvironment{a}[2]{-}{|}"
                      ++ "\\renewenvironment{a}{(}{)}"
                      ++ "\\begin{a}B\\end{a}"))
  ]

testsCatcode :: Test
testsCatcode = TestLabel "catcode" $ test
  [ "global catcode change"
    ~: [(TeXChar '#' Letter)]
    ~=? (lexTeX "" "\\catcode 35=11#")
  , "catcode changes have block-local scope"
    ~: [(TeXChar '$' Mathshift), (TeXChar '{' Bgroup), (TeXChar '$' Other),
        (TeXChar '}' Egroup), (TeXChar '$' Mathshift)]
    ~=? (lexTeX "" "${\\catcode`$=12$}$")
  , "add new escape character"
    ~: [(CtrlSeq "hello" False), (CtrlSeq "world" False), (TeXChar '!' Other)]
    ~=? (lexTeX "" "\\catcode`@=0\\hello@world!")
  , "add new block-local egroup character"
    ~: [(TeXChar '{' Bgroup), (TeXChar '{' Bgroup), (TeXChar '{' Bgroup),
        (TeXChar ')' Egroup), (TeXChar ')' Other), (TeXChar '}' Egroup),
        (TeXChar '}' Egroup)]
    ~=? (lexTeX "" "{{{\\catcode`)2))}}")
  , "add new group delimiters"
    ~: [(TeXChar '{' Bgroup), (TeXChar 'a' Letter), (TeXChar '<' Bgroup),
        (TeXChar '}' Egroup), (TeXChar '{' Bgroup), (TeXChar '>' Egroup),
        (TeXChar 'b' Letter), (TeXChar '>' Egroup), (TeXChar '>' Other)]
    ~=? (lexTeX "" "{\\catcode`>=2\\catcode`<1a<}{>b>>")
  , "allow colon in macro names"
    ~: [(CtrlSeq "hello:world" False), (TeXChar '!' Other)]
    ~=? (lexTeX "" "\\catcode`:=11\\hello:world!")
  , "copy catcode of another character"
    ~: [(TeXChar '%' Other)]
    ~=? (lexTeX "" "\\catcode`%=\\catcode`8%")
  ]

testsCatcodeInMacro :: Test
testsCatcodeInMacro = TestLabel "catcodes in macro" $ test
  [ "catcode change in macro body has global effect after call"
    ~: [(TeXChar '#' Letter)]
    ~=? (lexTeX "" "\\def\\a{\\catcode35=11}\\a #")
  , "catcode change in macro body has block-local effect after call"
    ~: [(TeXChar '{' Bgroup), (TeXChar '$' Other),
        (TeXChar '}' Egroup), (TeXChar '$' Mathshift)]
    ~=? (lexTeX "" "\\def\\a{\\catcode`$=12}{\\a $}$")
  ]

testsActiveChars :: Test
testsActiveChars = TestLabel "active characters" $ test
  [ "make character active"
    ~: [(CtrlSeq "|" True), (TeXChar 'b' Letter), (CtrlSeq "|" True),
        (CtrlSeq "|" False), (TeXChar '!' Other)]
    ~=? (lexTeX "" "\\catcode`|=13|b|\\|!")
  , "define macro for active character"
    ~: replicate 3 (TeXChar 'a' Letter)
    ~=? (lexTeX "" "\\catcode`|=13\\def|{a}|||")
  ]

testsChar :: Test
testsChar = TestLabel "char command" $ test
  [ "insert percent sign by number"
    ~: replicate 2 (TeXChar '%' Other)
    ~=? (lexTeX "" "\\char37\\char `\\%")
  , "insert backslash by number"
    ~: replicate 2 (TeXChar '\\' Other)
    ~=? (lexTeX "" "\\char 92\\char`\\\\")
  ]

testsNumber :: Test
testsNumber = TestLabel "number command" $ test
  [ "convert an internal hex number to dec tokens"
    ~: [(TeXChar '4' Other), (TeXChar '2' Other)]
    ~=? (lexTeX "" "\\number\"2A")
  , "print the character code of the percent sign"
    ~: [(TeXChar '3' Other), (TeXChar '7' Other)]
    ~=? (lexTeX "" "\\number`\\%")
  , "print the current catcode of the percent sign"
    ~: [(TeXChar '1' Other), (TeXChar '4' Other)]
    ~=? (lexTeX "" "\\number\\catcode 37")
  , "print the current catcode of the backslash character"
    ~: [(TeXChar '0' Other)]
    ~=? (lexTeX "" "\\number\\catcode`\\\\")
  ]

testsMeaning :: Test
testsMeaning = TestLabel "meaning command" $ test
  [ "meaning of a letter"
    ~: mkQuote "the letter a"
    ~=? (lexTeX "" "\\meaning a")
  , "meaning of a parameter character"
    ~: mkQuote "macro parameter character #"
    ~=? (lexTeX "" "\\meaning#")
  , "meaning of a control sequence that denotes a primitive command"
    ~: mkQuote "primitive:def"
    ~=? (lexTeX "" "\\meaning \\def")
  , "meaning of a user-defined macro"
    ~: mkQuote "macro:[Mandatory]->(#1)"
    ~=? (lexTeX "" "\\def\\a#1{(#1)}\\meaning\\a")
  , "meaning of an undefined macro"
    ~: mkQuote "undefined"
    ~=? (lexTeX "" "\\meaning\\a")
  , "meaning of the primitive \\undefined"
    ~: mkQuote "primitive:undefined"
    ~=? (lexTeX "" "\\meaning\\undefined")
  , "the meaning of meaning"
    ~: mkQuote "primitive:meaning"
    ~=? (lexTeX "" "\\meaning\\meaning")
  ]

-- these tests run with 'lexTeX' over the identity monad, so @\\input@
-- and @\\include@ commands are ignored, but we can test the filename parser.
testsIncludes :: Test
testsIncludes = TestLabel "include commands" $ test
  [ "input with whitespace-separated ascii filename"
    ~: mkString "ab"
    ~=? (lexTeX "" "\\input hello ab")
  , "input with special characters in filename"
    ~: mkString "ab"
    ~=? (lexTeX "" "\\input z$&/#^_~.#!~ ab")
  , "include with whitespace in group argument"
    ~: mkString "ab"
    ~=? (lexTeX "" "\\include{ hello }ab")
  , "include without whitespace in group argument"
    ~: mkString "ab"
    ~=? (lexTeX "" "\\include{h$ll#}ab")
  ]

-- collect all tests
tests :: Test
tests = TestList
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

-- run tests
main :: IO ExitCode
main = do
  counts <- runTestTT tests
  if errors counts == 0 && failures counts == 0
    then exitSuccess
    else exitFailure
