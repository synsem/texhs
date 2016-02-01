{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
--
-- Module      :  Text.BibSpec
-- Copyright   :  2015-2016 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
-- Maintainer  :  mathias.schenner@langsci-press.org
--
-- Tests for the "Text.Bib" module.
----------------------------------------------------------------------

module Text.BibSpec
  ( tests
  ) where

import Data.List (intersperse)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

import Text.Bib
import Text.Bib.Types
import Text.Bib.Reader.BibTeX.Reference (parseAgents, parseList)
import Text.Doc.Types
import Text.Doc.Reader.TeX (tex2inlines)
import Text.TeX (readTeX)


-------------------- tests

tests :: Test
tests = testGroup "Text.BibSpec"
  [ testsNames
  , testsLiteralList
  , testsEntry
  , testsNameConflicts
  , testsMacro
  , testsInheritanceXData
  , testsInheritanceCrossref
  , testsInheritance
  , testsNormalize
  , testsFormatter
  ]

testsNames :: Test
testsNames = testGroup "name lists"
  [ testCase "First Last" $
    parseAgents (readTeX "" "First Last")
    @?=
    [Agent [Str "First"] [] [Str "Last"] []]
  , testCase "First von Last" $
    parseAgents (readTeX "" "First von Last")
    @?=
    [Agent [Str "First"] [Str "von"] [Str "Last"] []]
  , testCase "von Last" $
    parseAgents (readTeX "" "von Last")
    @?=
    [Agent [] [Str "von"] [Str "Last"] []]
  , testCase "von last" $
    -- BibTeX warning: lower-case last name
    parseAgents (readTeX "" "von last")
    @?=
    [Agent [] [Str "von"] [Str "last"] []]
  , testCase "Last" $
    parseAgents (readTeX "" "Last")
    @?=
    [Agent [] [] [Str "Last"] []]
  , testCase "last" $
    -- BibTeX warning: lower-case last name
    parseAgents (readTeX "" "last")
    @?=
    [Agent [] [] [Str "last"] []]
  , testCase "First von last" $
    -- BibTeX warning: lower-case last name
    parseAgents (readTeX "" "First von last")
    @?=
    [Agent [Str "First"] [Str "von"] [Str "last"] []]
  , testCase "First von von last" $
    -- BibTeX warning: lower-case last name
    parseAgents (readTeX "" "First von von last")
    @?=
    [Agent [Str "First"] [Str "von", Space, Str "von"] [Str "last"] []]
  , testCase "First von Last last Last" $
    parseAgents (readTeX "" "First von Last last Last")
    @?=
    [Agent [Str "First"] [Str "von"]
      [Str "Last", Space, Str "last", Space, Str "Last"] []]
  , testCase "First First Last" $
    parseAgents (readTeX "" "First First Last")
    @?=
    [Agent [Str "First", Space, Str "First"] [] [Str "Last"] []]
  , testCase "Last Last, First" $
    parseAgents (readTeX "" "Last Last, First")
    @?=
    [Agent [Str "First"] [] [Str "Last", Space, Str "Last"] []]
  , testCase "{Last, last}" $
    -- Comma at brace level 1 is invisible during name-breaking.
    parseAgents (readTeX "" "{Last, last}")
    @?=
    [Agent [] [] [Str "Last,", Space, Str "last"] []]
  , testCase "{Last{, }last}" $
    -- Comma at brace level 2 is invisible during name-breaking.
    parseAgents (readTeX "" "{Last{, }last}")
    @?=
    [Agent [] [] [Str "Last,", Space, Str "last"] []]
  , testCase "last, First" $
    -- BibTeX warning: lower-case last name
    parseAgents (readTeX "" "last, First")
    @?=
    [Agent [Str "First"] [] [Str "last"] []]
  , testCase "{von last}, First" $
    -- BibTeX warning: lower-case last name
    parseAgents (readTeX "" "{von last}, First")
    @?=
    [Agent [Str "First"] [] [Str "von", Space, Str "last"] []]
  , testCase "von Last Last, First first" $
    parseAgents (readTeX "" "von Last Last, First first")
    @?=
    [Agent [Str "First", Space, Str "first"] [Str "von"]
      [Str "Last", Space, Str "Last"] []]
  , testCase "von Last, Jr, First" $
    parseAgents (readTeX "" "von Last, Jr, First")
    @?=
    [Agent [Str "First"] [Str "von"] [Str "Last"] [Str "Jr"]]
  , testCase "von Last, Jr, First, First" $
    -- BibTeX warning: too many commas
    parseAgents (readTeX "" "von Last, Jr, First, First")
    @?=
    [Agent (intersperse Space (replicate 2 (Str "First")))
      [Str "von"] [Str "Last"] [Str "Jr"]]
  , testCase "von Last Last, Junior, First, First First, First" $
    -- BibTeX warning: too many commas
    parseAgents (readTeX ""
      "von Last Last, Junior, First, First First, First")
    @?=
    [Agent (intersperse Space (replicate 4 (Str "First")))
      [Str "von"] [Str "Last", Space, Str "Last"] [Str "Junior"]]
  , testCase "First Last and von Last, Jr, First, First" $
    -- BibTeX warning: too many commas
    parseAgents (readTeX ""
      "First Last and von Last, Jr, First, First")
    @?=
    [ Agent [Str "First"] [] [Str "Last"] []
    , Agent (intersperse Space (replicate 2 (Str "First")))
      [Str "von"] [Str "Last"] [Str "Jr"]]
  , testCase "First Last and First Last" $
    parseAgents (readTeX "" "First Last and First Last")
    @?=
    replicate 2 (Agent [Str "First"] [] [Str "Last"] [])
  , testCase "First von Last and von Last, First" $
    parseAgents (readTeX "" "First von Last and von Last, First")
    @?=
    replicate 2 (Agent [Str "First"] [Str "von"] [Str "Last"] [])
  , testCase "one name, no commas, one 'von' part" $
    parseAgents (readTeX "" "John von Neumann")
    @?=
    [Agent [Str "John"] [Str "von"] [Str "Neumann"] []]
  , testCase "one name, one comma, one 'von' part" $
    parseAgents (readTeX "" "von Neumann, John")
    @?=
    [Agent [Str "John"] [Str "von"] [Str "Neumann"] []]
  , testCase "one name, one comma, two last names" $
    parseAgents (readTeX "" "Brinch Hansen, Per")
    @?=
    [Agent [Str "Per"] [] [Str "Brinch", Space, Str "Hansen"] []]
  , testCase "two names, no commas, no 'von' part" $
    parseAgents (readTeX "" "Alfred North Whitehead and Bertrand Russell")
    @?=
    [ Agent [Str "Alfred", Space, Str "North"] [] [Str "Whitehead"] []
    , Agent [Str "Bertrand"] [] [Str "Russell"] []]
  , testCase "one name, no commas, no 'von' part, with precomposed umlaut char" $
    parseAgents (readTeX "" "Kurt Gödel")
    @?=
    [Agent [Str "Kurt"] [] [Str "Gödel"] []]
  , testCase "one name, no commas, no 'von' part, with redundant whitespace" $
    parseAgents (readTeX "" " Kurt  Gödel ")
    @?=
    [Agent [Str "Kurt"] [] [Str "Gödel"] []]
  , testCase "one name, one comma, no 'von' part, with redundant whitespace in braces" $
    parseAgents (readTeX "" "{ Gödel }, Kurt")
    @?=
    [Agent [Str "Kurt"] [] [Str "Gödel"] []]
  , testCase "one name, one comma, no 'von' part, with TeX-escaped special char" $
    parseAgents (readTeX "" "B{\\\"u}chner, Georg")
    @?=
    [Agent [Str "Georg"] [] [Str "Bu\x0308\&chner"] []]
  , testCase "one company name containing hidden 'and' (at brace level 1)" $
    parseAgents (readTeX "" "{Nobody and Sons, Inc.}")
    @?=
    [Agent [] [] (toInlines "{Nobody and Sons, Inc.}") []]
  , testCase "two names, combining one visible and one hidden 'and'" $
    parseAgents (readTeX "" "{National Aeronautics and Space Administration} and Doe, John")
    @?=
    [ Agent [] [] (toInlines "{National Aeronautics and Space Administration}") []
    , Agent [Str "John"] [] [Str "Doe"] []]
  , testCase "one name, no commas, multiple first, prefix and last names" $
    parseAgents (readTeX "" "Charles Louis Xavier Joseph de la Vall\\'ee Poussin")
    @?=
    [Agent [Str "Charles", Space, Str "Louis", Space, Str "Xavier", Space, Str "Joseph"]
      [Str "de", Space, Str "la"] [Str "Valle\x0301\&e", Space, Str "Poussin"] []]
  , testCase "one name, no commas, with non-breaking spaces" $
    parseAgents (readTeX "" "Charles Louis Xavier~Joseph de~la Vall\\'ee~Poussin")
    @?=
    [Agent [Str "Charles", Space, Str "Louis", Space, Str ("Xavier" ++ "\x00A0" ++ "Joseph")]
      [Str ("de" ++ "\x00A0" ++ "la")] [Str ("Valle\x0301\&e" ++ "\x00A0" ++ "Poussin")] []]
  ]

testsLiteralList :: Test
testsLiteralList = testGroup "literal lists"
  [ testCase "zero items" $
    parseList (readTeX "" "")
    @?=
    []
  , testCase "one item" $
    parseList (readTeX "" "one")
    @?=
    [toInlines "one"]
  , testCase "two items" $
    parseList (readTeX "" "one and two")
    @?=
    map toInlines ["one", "two"]
  , testCase "three items" $
    parseList (readTeX "" "one and two and three")
    @?=
    map toInlines ["one", "two", "three"]
  , testCase "one item, with hidden 'and' in outer group" $
    parseList (readTeX "" "{one and two}")
    @?=
    [toInlines "{one and two}"]
  , testCase "one item, with hidden 'and' in inner group" $
    parseList (readTeX "" "one {and} two")
    @?=
    [toInlines "one {and} two"]
  , testCase "two items, combining visible and hidden 'and'" $
    parseList (readTeX "" "{one and one} and two")
    @?=
    map toInlines ["{one and one}", "two"]
  , testCase "delimiter 'and' must be surrounded by whitespace" $
    parseList (readTeX "" "land and andy dandy")
    @?=
    map toInlines ["land", "andy dandy"]
  , testCase "delimiter 'and' must be surrounded by whitespace" $
    parseList (readTeX "" "{d}and{y}")
    @?=
    [toInlines "dandy"]
  , testCase "delimiter 'and' must be surrounded by whitespace" $
    parseList (readTeX "" "{d} and{y}")
    @?=
    [toInlines "{d} and{y}"]
  , testCase "delimiter 'and' must be surrounded by whitespace" $
    parseList (readTeX "" "{d}and {y}")
    @?=
    [toInlines "{d}and {y}"]
  , testCase "delimiter 'and' must be surrounded by whitespace" $
    parseList (readTeX "" "{d} and {y}")
    @?=
    map toInlines ["{d}", "{y}"]
  , testCase "isolated 'and'" $
    parseList (readTeX "" "and")
    @?=
    []
  , testCase "double isolated 'and'" $
    parseList (readTeX "" "and and")
    @?=
    []
  , testCase "mixing visible and hidden 'and'" $
    parseList (readTeX "" "and {and} and {and and} and {and}")
    @?=
    map toInlines ["{and}", "{and and}", "{and}"]
  , testCase "empty left-hand side" $
    parseList (readTeX "" "and one")
    @?=
    [toInlines "one"]
  , testCase "empty right-hand side" $
    parseList (readTeX "" "one and")
    @?=
    [toInlines "one"]
  , testCase "white left-hand side" $
    parseList (readTeX "" " and one")
    @?=
    [toInlines "one"]
  , testCase "white right-hand side" $
    parseList (readTeX "" "one and ")
    @?=
    [toInlines "one"]
  , testCase "'others' on right-hand side" $
    -- We do not (yet) treat 'others' in a special way.
    parseList (readTeX "" "one and others")
    @?=
    map toInlines ["one", "others"]
  ]

testsEntry :: Test
testsEntry = testGroup "parse bib entry"
  [ testCase "empty entry with braces" $
    fromBibTeX "" "@book{b,}"
    @?=
    mkBibDB [mkBookEntry "b" []]
  , testCase "empty entry with parens" $
    fromBibTeX "" "@book(b,)"
    @?=
    mkBibDB [mkBookEntry "b" []]
  , testCase "valid empty entries with mismatching delimiters" $
    -- BibTeX warning: entry started with X but ends with Y
    fromBibTeX "" "@book{b1,)@book(b2,}"
    @?=
    mkBibDB [mkBookEntry "b1" [], mkBookEntry "b2" []]
  , testCase "simple entry" $
    fromBibTeX "" bibFile01
    @?=
    mkBibDB [bibEntry01]
  , testCase "simple entry with hash-separated subfields" $
    fromBibTeX "" bibFile01a
    @?=
    mkBibDB [bibEntry01]
  , testCase "entrykey (aka citekey) may start with a digit (unlike field names)" $
    fromBibTeX "" "@book{4i , }"
    @?=
    mkBibDB [mkBookEntry "4i" []]
  , testCase "entrykey (aka citekey) may be numeric (unlike field names)" $
    fromBibTeX "" "@book{23,}"
    @?=
    mkBibDB [mkBookEntry "23" []]
  , testCase "do not strip whitespace around inner group" $
    fromBibTeX "" "@book{b, title = {a {b} c }}"
    @?=
    mkBibDB [mkBookEntry "b"
      [("title", [ Str "a", Space, Str "b", Space, Str "c" ])]]
  , testCase "do not add whitespace around inner group" $
    fromBibTeX "" "@book{b, title = { a{b}c }}"
    @?=
    mkBibDB [mkBookEntry "b"
      [("title", [ Str "abc" ])]]
  , testCase "do not strip whitespace after inner group" $
    fromBibTeX "" "@book{b, title = {a{b} c }}"
    @?=
    mkBibDB [mkBookEntry "b"
      [("title", [ Str "ab", Space, Str "c" ])]]
  , testCase "do not strip inner whitespace" $
    fromBibTeX "" "@book{b, eq = 2 # { + } #2 # { } # \"= \" # {4}}"
    @?=
    mkBibDB [mkBookEntry "b"
      [("eq", [ Str "2", Space, Str "+", Space, Str "2"
              , Space, Str "=", Space, Str "4"])]]
  , testCase "do not strip inner whitespace" $
    fromBibTeX "" "@book{b, eq = 2#{ }#\"+\"# {   } #2}"
    @?=
    mkBibDB [mkBookEntry "b"
      [("eq", [Str "2", Space, Str "+", Space, Str "2"])]]
  , testCase "conflate inner whitespace" $
    fromBibTeX "" "@book{b, eq = 2 # { + } # {  } # { } #2 }"
    @?=
    mkBibDB [mkBookEntry "b"
      [("eq", [Str "2", Space, Str "+", Space, Str "2"])]]
  , testCase "strip outer whitespace" $
    fromBibTeX "" "@book{b, title = { } # { White } # { Space } # { } # { } }"
    @?=
    mkBibDB [mkBookEntry "b"
      [("title", [Str "White", Space, Str "Space"])]]
  , testCase "strip outer but not inner whitespace" $
    fromBibTeX "" "@book{b,title = { ab } # 22 # \" cd \"}"
    @?=
    mkBibDB [mkBookEntry "b"
      [("title", [Str "ab", Space, Str "22", Space, Str "cd"])]]
  ]

testsNameConflicts :: Test
testsNameConflicts = testGroup "citekey and fieldname conflicts"
  [ testCase "duplicate citekeys: first one wins" $
    -- BibTeX warning: duplicate entry key
    fromBibTeX "" (T.concat
      [ "@book{b, title={one}}"
      , "@book{b, title={two}}"
      , "@book{b, title={three}}"])
    @?=
    mkBibDB [mkBookEntry "b" [("title", [Str "one"])]]
  , testCase "duplicate fieldnames: last one wins" $
    -- Note: biber-2.2 does not warn about duplicate field names
    fromBibTeX "" "@book{b,title={one},title={two},title={three}}"
    @?=
    mkBibDB [mkBookEntry "b" [("title", [Str "three"])]]
  , testCase "citekeys are case-sensitive" $
    fromBibTeX "" (T.concat
      [ "@book{b, title={b-one}}"
      , "@book{B, title={B-one}}"
      , "@book{b, title={b-two}}"])
    @?=
    mkBibDB [ mkBookEntry "b" [("title", [Str "b-one"])]
            , mkBookEntry "B" [("title", [Str "B-one"])]]
  , testCase "fieldnames are case-insensitive" $
    fromBibTeX "" "@book{b,title={one},TITLE={two},Title={three}}"
    @?=
    mkBibDB [mkBookEntry "b" [("title", [Str "three"])]]
  , testCase "mixing duplicate citekeys and fieldnames" $
    -- BibTeX warning: duplicate entry key
    fromBibTeX "" (T.concat
      [ "@book{b,title={1-1},title={1-2},title={1-3}}"
      , "@book{b,title={2-1},title={2-2},title={2-3}}"
      , "@book{b,title={3-1},title={3-2},title={3-3}}"])
    @?=
    mkBibDB [mkBookEntry "b" [("title", [Str "1-3"])]]
  ]

testsMacro :: Test
testsMacro = testGroup "resolve bibtex @string macros"
  [ testCase "using built-in month macros" $
    fromBibTeX "" "@book{b, month = aug, other=dec}"
    @?=
    mkBibDB [mkBookEntry "b" [("month", [Str "8"]), ("other", [Str "12"])]]
  , testCase "simple macro use" $
    fromBibTeX "" "@string{v={some text}}@book{b, k=v, i=v}"
    @?=
    let r = [Str "some", Space, Str "text"]
    in mkBibDB [mkBookEntry "b" [("k", r), ("i", r)]]
  , testCase "macro definition with subfields" $
    fromBibTeX "" "@string{v = 2 # \"+\" #2#{=4}} @book{b, eq=v}"
    @?=
    mkBibDB [mkBookEntry "b" [("eq", [Str "2+2=4"])]]
  , testCase "macro use with subfields" $
    fromBibTeX "" "@string{v={ab}}@book{b, title = v #{-}# v #{.} }"
    @?=
    mkBibDB [mkBookEntry "b" [("title", [Str "ab-ab."])]]
  , testCase "nested macro definition" $
    fromBibTeX "" (T.concat
      [ " @string{s1={a}} "
      , " @string{s2=s1} "
      , " @book{b, v=s2}"])
    @?=
    mkBibDB [mkBookEntry "b" [("v", [Str "a"])]]
  , testCase "nested macro definition with subfields" $
    fromBibTeX "" (T.concat
      [ " @string{s1={a}} "
      , " @string{s2 = s1 # s1# s1} "
      , " @book{b, v=s2}"])
    @?=
    mkBibDB [mkBookEntry "b" [("v", [Str "aaa"])]]
  , testCase "field names are not subject to expansion" $
    fromBibTeX "" "@string{v={repl}}@book{b, v=v}"
    @?=
    mkBibDB [mkBookEntry "b" [("v", [Str "repl"])]]
  , testCase "overwriting macro" $
    -- BibTeX warning: overwritten macro
    fromBibTeX "" (T.concat
      [ "@string{s1 = {a}}"
      , "@string{s1 = s1 # {b}}"
      , "@string{s1 = s1 # {c}}"
      , "@book{b, title = s1}"])
    @?=
    mkBibDB [mkBookEntry "b" [("title", [Str "abc"])]]
  , testCase "overwriting built-in month macros" $
    -- BibTeX warning: overwritten macro
    fromBibTeX "" (T.concat
      [ "@book{b1, month = feb}"
      , "@string{feb = {Hornung}}"
      , "@book{b2, month = feb}"])
    @?=
    mkBibDB [ mkBookEntry "b1" [("month", [Str "2"])]
            , mkBookEntry "b2" [("month", [Str "Hornung"])]]
  , testCase "no self-reference" $
    -- BibTeX warning: undefined macro
    fromBibTeX "" "@string{me=1#me}@book{b, v=me}"
    @?=
    mkBibDB [mkBookEntry "b" [("v", [Str "1"])]]
  , testCase "numeric plain fields" $
    fromBibTeX "" "@book{b, year = 2525, volume = 30}"
    @?=
    mkBibDB [mkBookEntry "b" [("year", [Str "2525"]), ("volume", [Str "30"])]]
  , testCase "negative numeric plain field (treated as macro name)" $
    -- BibTeX warning: undefined macro
    fromBibTeX "" "@book{b, volume = -1}"
    @?=
    mkBibDB [mkBookEntry "b" [("volume", [])]]
  , testCase "negative numeric braced field" $
    fromBibTeX "" "@book{b, volume = {-1}}"
    @?=
    mkBibDB [mkBookEntry "b" [("volume", [Str "-1"])]]
  , testCase "macro names (field names) must not start with a digit" $
    -- BibTeX error: biber-2.2 throws syntax error.
    -- By contrast, we simply drop any leading digits and continue.
    fromBibTeX "" "@string{2={two}}@book{b,v=2}"
    @?=
    mkBibDB [mkBookEntry "b" [("v", [Str "2"])]]
  , testCase "macro names (field names) may consist of symbols only" $
    fromBibTeX "" (T.concat
      [ "@string{*={star}}"
      , "@string{+*+={+}}"
      , "@book{b,stars=*#+*+#*}"])
    @?=
    mkBibDB [mkBookEntry "b" [("stars", [Str "star+star"])]]
  , testCase "macro names (field names) are case-insensitive" $
    fromBibTeX "" "@string{ab={aB}}@book{b,v= AB#aB#Ab#ab}"
    @?=
    mkBibDB [mkBookEntry "b" [("v", [Str "aBaBaBaB"])]]
  , testCase "undefined macro" $
    -- BibTeX warning: undefined macro
    fromBibTeX "" "@book{b,v=ab#{-}#-#cd#4}"
    @?=
    mkBibDB [mkBookEntry "b" [("v", [Str "-4"])]]
  ]

testsInheritanceXData :: Test
testsInheritanceXData = testGroup "xdata inheritance"
  [ testCase "remove xdata entries from bibdb" $
    fromBibTeX "" "@xdata{x1, location = {here}}"
    @?=
    mkBibDB []
  , testCase "simple xdata inheritance" $
    fromBibTeX "" (T.concat
      [ "@xdata{x1, title = {x1 title}, location = {Berlin}}"
      , "@book{b1, author={b1-author}, xdata={x1}}"])
    @?=
    mkBibDB [mkEntry "b1" "book"
      [ ("author", AgentList [Agent [] [] [Str "b1-author"] []])
      , ("title", LiteralField [Str "x1", Space, Str "title"])
      , ("location", LiteralList [[Str "Berlin"]])]]
  , testCase "simple xdata inheritance, reversed entry order in source" $
    fromBibTeX "" (T.concat
      [ "@book{b1, author={b1-author}, xdata={x1}}"
      , "@xdata{x1, title = {x1 title}, location = {Berlin}}"])
    @?=
    mkBibDB [mkEntry "b1" "book"
      [ ("author", AgentList [Agent [] [] [Str "b1-author"] []])
      , ("title", LiteralField [Str "x1", Space, Str "title"])
      , ("location", LiteralList [[Str "Berlin"]])]]
  , testCase "nested xdata inheritance" $
    -- "b" --(xdata)-> "x1-0" --(xdata)-> "x1-1"
    fromBibTeX "" (T.concat
      [ "@xdata{x1-0, level1 = {x1-0}, xdata={x1-1}}"
      , "@book{b, level0 = {b}, xdata={x1-0}}"
      , "@xdata{x1-1, level2 = {x1-1}}"])
    @?=
    mkBibDB [mkBookEntry "b"
      [ ("level0", [Str "b"])
      , ("level1", [Str "x1-0"])
      , ("level2", [Str "x1-1"])]]
  , testCase "multiple xdata inheritance" $
    -- "b" --(xdata)-> "x1"
    -- "b" --(xdata)-> "x2"
    -- Note: we assume that "xdata1" and "xdata2" have no internal meaning.
    fromBibTeX "" (T.concat
      [ "@xdata{x2, xdata2={x2}}"
      , "@book{b, self = {b}, xdata={x1,x2}}"
      , "@xdata{x1, xdata1 = {x1}}"])
    @?=
    mkBibDB [mkBookEntry "b"
      [ ("self", [Str "b"])
      , ("xdata1", [Str "x1"])
      , ("xdata2", [Str "x2"])]]
  , testCase "multiple nested xdata inheritance" $
    -- "b" --(xdata)-> "x1" --(xdata)-> "x1-1"
    -- "b" --(xdata)-> "x2" --(xdata)-> "x2-1"
    fromBibTeX "" (T.concat
      [ "@book{b, self = {b}, xdata={x1,x2}}"
      , "@xdata{x1, xdata={x1-1}}"
      , "@xdata{x1-1, first={x1-1}}"
      , "@xdata{x2, xdata = {x2-1}}"
      , "@xdata{x2-1, second = {x2-1}}"])
    @?=
    mkBibDB [mkBookEntry "b"
      [ ("self", [Str "b"])
      , ("first", [Str "x1-1"])
      , ("second", [Str "x2-1"])]]
  , testCase "name conflicts in xdata-inherited fields: right-biased resolution" $
    fromBibTeX "" (T.concat
      [ "@book{b1, xdata={x2,x1}}"
      , "@book{b2, xdata={x1,x2}}"
      , "@xdata{x1, title = {x1}}"
      , "@xdata{x2, title = {x2}}"])
    @?=
    mkBibDB [ mkBookEntry "b1" [("title", [Str "x1"])]
            , mkBookEntry "b2" [("title", [Str "x2"])]]
  , testCase "inherited fields do not overwrite existing fields" $
    fromBibTeX "" (T.concat
      [ "@book{b1, xdata={x1,x2}, title = {self}}"
      , "@book{b2, title = {self}, xdata={x1,x2}}"
      , "@xdata{x1, title = {x1}}"
      , "@xdata{x2, title = {x2}}"])
    @?=
    mkBibDB [ mkBookEntry "b1" [("title", [Str "self"])]
            , mkBookEntry "b2" [("title", [Str "self"])]]
  , testCase "xdata inheritance from non-xdata entries is possible" $
    -- biber-2.2 allows this without warning
    fromBibTeX "" (T.concat
      [ "@book{x1, title = {x1}}"
      , "@book{b1, author={b1-author}, xdata={x1}}"])
    @?=
    mkBibDB
      [ mkEntry "x1" "book" -- an xdata entry in disguise
        [ ("title", LiteralField [Str "x1"])]
      , mkEntry "b1" "book"
        [ ("author", AgentList [Agent [] [] [Str "b1-author"] []])
        , ("title", LiteralField [Str "x1"])]]
  , testCase "avoid direct reference cycles" $
    -- biber-2.2 error: Circular XDATA inheritance
    -- "x1" <--(xdata)--> "x1"
    fromBibTeX "" (T.concat
      [ "@xdata{x1, xdata={x1}}"
      , "@book{b1, xdata={x1}}"])
    @?=
    mkBibDB [mkBookEntry "b1" []]
  , testCase "avoid indirect reference cycles" $
    -- biber-2.2 error: Circular XDATA inheritance
    -- "x1" <--(xdata)--> "x2"
    fromBibTeX "" (T.concat
      [ "@xdata{x1, xdata={x2}}"
      , "@xdata{x2, xdata={x1}}"
      , "@book{b1, xdata={x1}}"])
    @?=
    mkBibDB [mkBookEntry "b1" []]
  , testCase "avoid indirect reference cycles" $
    -- biber-2.2 error: Circular XDATA inheritance
    -- "x1" <--(xdata)--> "x3"
    fromBibTeX "" (T.concat
      [ "@xdata{x1, xdata={x2}}"
      , "@xdata{x2, xdata={x3}}"
      , "@xdata{x3, xdata={x1}}"
      , "@book{b1, xdata={x1}}"])
    @?=
    mkBibDB [mkBookEntry "b1" []]
  , testCase "avoid reference cycles" $
    -- biber-2.2 error: Circular XDATA inheritance
    -- "x1" <--(xdata)--> "x1" <--(xdata)--> "x2" <--(xdata)--> "x2"
    fromBibTeX "" (T.concat
      [ "@xdata{x1, xdata={x1,x2}}"
      , "@xdata{x2, xdata={x1,x2}}"
      , "@book{b1, xdata={x1,x2}}"])
    @?=
    mkBibDB [mkBookEntry "b1" []]
  , testCase "xdata-inheritance does not rename fields" $
    -- ... even if the referenced entry is not actually an xdata entry
    fromBibTeX "" (T.concat
      [ "@incollection{i1, xdata={c1}}"
      , "@collection{c1, title={c1-title}}"])
    @?=
    mkBibDB
      [ mkEntry "i1" "incollection"
        [ ("title", LiteralField [Str "c1-title"]) ] -- not "booktitle"
      , mkEntry "c1" "collection" -- an xdata entry in disguise
        [ ("title", LiteralField [Str "c1-title"]) ]]
  ]

testsInheritanceCrossref :: Test
testsInheritanceCrossref = testGroup "crossref inheritance"
  [ testCase "simple crossref inheritance" $
    fromBibTeX "" (T.concat
      [ "@InCollection{i1, title={i1 title}, crossref={c1}}"
      , "@COLLECTION{c1, location={Berlin}}"])
    @?=
    mkBibDB
      [ mkEntry "i1" "incollection"
        [ ("title", LiteralField [Str "i1", Space, Str "title"])
        , ("crossref", RawField "c1")
        , ("location", LiteralList [[Str "Berlin"]]) ]
      , mkEntry "c1" "collection"
        [ ("location", LiteralList [[Str "Berlin"]]) ]]
  , testCase "nested crossref inheritance" $
    fromBibTeX "" (T.concat
      [ "@incollection{i1, crossref={c1}, title={i1-title}}"
      , "@collection{c1, booktitle={c1-booktitle}, crossref={c2}}"
      , "@collection{c2, location={Berlin}}"])
    @?=
    mkBibDB
      [ mkEntry "i1" "incollection"
        [ ("title", LiteralField [Str "i1-title"])
        , ("crossref", RawField "c1")
        , ("booktitle", LiteralField [Str "c1-booktitle"])
        , ("location", LiteralList [[Str "Berlin"]]) ]
      , mkEntry "c1" "collection"
        [ ("booktitle", LiteralField [Str "c1-booktitle"])
        , ("crossref", RawField "c2")
        , ("location", LiteralList [[Str "Berlin"]]) ]
      , mkEntry "c2" "collection"
        [ ("location", LiteralList [[Str "Berlin"]]) ]]
  , testCase "rename selected fields" $
    fromBibTeX "" (T.concat
      [ "@incollection{i1, crossref={c1}, title={i1-title}}"
      , "@collection{c1, title={c1-title}}"])
    @?=
    mkBibDB
      [ mkEntry "i1" "incollection"
        [ ("title", LiteralField [Str "i1-title"])
        , ("crossref", RawField "c1")
        , ("booktitle", LiteralField [Str "c1-title"]) ]
      , mkEntry "c1" "collection"
        [ ("title", LiteralField [Str "c1-title"]) ]]
  , testCase "rename selected fields" $
    fromBibTeX "" (T.concat
      [ "@incollection{i1, crossref={c1}}"
      , "@collection{c1, title={c1-title}}"])
    @?=
    mkBibDB
      [ mkEntry "i1" "incollection"
        [ ("booktitle", LiteralField [Str "c1-title"]) -- not "title"
        , ("crossref", RawField "c1") ]
      , mkEntry "c1" "collection"
        [ ("title", LiteralField [Str "c1-title"]) ]]
  , testCase "do not overwrite existing fields" $
    fromBibTeX "" (T.concat
      [ "@incollection{i1, booktitle={i1-booktitle},"
      , "  crossref={c1}, title={i1-title}}"
      , "@collection{c1, title={c1-title}}"])
    @?=
    mkBibDB
      [ mkEntry "i1" "incollection"
        [ ("title", LiteralField [Str "i1-title"])
        , ("crossref", RawField "c1")
        , ("booktitle", LiteralField [Str "i1-booktitle"]) ]
      , mkEntry "c1" "collection"
        [ ("title", LiteralField [Str "c1-title"]) ]]
  , testCase "do not inherit private fields" $
    fromBibTeX "" (T.concat
      [ "@incollection{i1, crossref={c1}, title={i1-title}}"
      , "@collection{c1, title={c1-title}, label={c1-label}, xref={none}}"])
    @?=
    mkBibDB
      [ mkEntry "i1" "incollection"
        [ ("title", LiteralField [Str "i1-title"])
        , ("crossref", RawField "c1")
        , ("booktitle", LiteralField [Str "c1-title"]) ]
      , mkEntry "c1" "collection"
        [ ("title", LiteralField [Str "c1-title"])
        , ("label", LiteralField [Str "c1-label"])
        , ("xref", LiteralField [Str "none"]) ]]
  , testCase "renamed fields overwrite existing fields in parent" $
    -- with biber-2.2: if there are both "title" and "booktitle" fields in the parent,
    -- then the child inherits the "title" field under the name of "booktitle".
    fromBibTeX "" (T.concat
      [ "@incollection{i1, crossref={c1}}"
      , "@collection{c1, booktitle={c1-booktitle}, title={c1-title}}"])
    @?=
    mkBibDB
      [ mkEntry "i1" "incollection"
        [ ("crossref", RawField "c1")
        , ("booktitle", LiteralField [Str "c1-title"]) ]
      , mkEntry "c1" "collection"
        [ ("booktitle", LiteralField [Str "c1-booktitle"])
        , ("title", LiteralField [Str "c1-title"]) ]]
  , testCase "renamed fields overwrite existing fields in parent" $
    -- same as previous test but with reversed field order in source,
    -- i.e. "title" before "booktitle".
    fromBibTeX "" (T.concat
      [ "@incollection{i1, crossref={c1}}"
      , "@collection{c1, title={c1-title}, booktitle={c1-booktitle}}"])
    @?=
    mkBibDB
      [ mkEntry "i1" "incollection"
        [ ("crossref", RawField "c1")
        , ("booktitle", LiteralField [Str "c1-title"]) ]
      , mkEntry "c1" "collection"
        [ ("booktitle", LiteralField [Str "c1-booktitle"])
        , ("title", LiteralField [Str "c1-title"]) ]]
  , testCase "parent booktitle field is inherited if it has no title field" $
    fromBibTeX "" (T.concat
      [ "@incollection{i1, crossref = {c1}}"
      , "@collection{c1, booktitle = {c1-booktitle}}"])
    @?=
    mkBibDB
      [ mkEntry "i1" "incollection"
        [ ("crossref", RawField "c1")
        , ("booktitle", LiteralField [Str "c1-booktitle"]) ]
      , mkEntry "c1" "collection"
        [ ("booktitle", LiteralField [Str "c1-booktitle"]) ]]
  , testCase "renamed fields do not overwrite existing fields in child" $
    fromBibTeX "" (T.concat
      [ "@incollection{i1, booktitle={i1-booktitle}, crossref={c1}}"
      , "@collection{c1, booktitle={c1-booktitle}, title={c1-title}}"])
    @?=
    mkBibDB
      [ mkEntry "i1" "incollection"
        [ ("crossref", RawField "c1")
        , ("booktitle", LiteralField [Str "i1-booktitle"]) ]
      , mkEntry "c1" "collection"
        [ ("booktitle", LiteralField [Str "c1-booktitle"])
        , ("title", LiteralField [Str "c1-title"]) ]]
  , testCase "avoid direct reference cycles" $
    -- biber-2.2 error: Circular inheritance
    -- "i1" <--(crossref)--> "i1"
    fromBibTeX "" "@incollection{i1, crossref={i1}}"
    @?=
    mkBibDB
      [ mkEntry "i1" "incollection"
        [("crossref", RawField "i1")]]
  , testCase "avoid indirect reference cycles" $
    -- biber-2.2 error: Circular inheritance
    -- "i1" <--(crossref)--> "i2"
    fromBibTeX "" (T.concat
      [ "@incollection{i1, crossref={i2}}"
      , "@incollection{i2, crossref={i1}}"])
    @?=
    mkBibDB
      [ mkEntry "i1" "incollection"
        [("crossref", RawField "i2")]
      , mkEntry "i2" "incollection"
        [("crossref", RawField "i1")]]
  ]

testsInheritance :: Test
testsInheritance = testGroup "xdata and crossref interaction"
  [ testCase "xdata fields win over crossref fields" $
    fromBibTeX "" (T.concat
      [ "@xdata{x1, booktitle={x1-booktitle}}"
      , "@incollection{i1, crossref={c1}, xdata={x1}}"
      , "@collection{c1, title={c1-title}}"])
    @?=
    mkBibDB
      [ mkEntry "i1" "incollection"
        [ ("crossref", RawField "c1")
        , ("booktitle", LiteralField [Str "x1-booktitle"]) ]
      , mkEntry "c1" "collection"
        [ ("title", LiteralField [Str "c1-title"]) ]]
  , testCase "xdata fields win over crossref fields" $
    -- same as previous test but with reversed field order in source,
    -- i.e. "xdata" before "crossref".
    fromBibTeX "" (T.concat
      [ "@xdata{x1, booktitle={x1-booktitle}}"
      , "@incollection{i1, xdata={x1}, crossref={c1}}"
      , "@collection{c1, title={c1-title}}"])
    @?=
    mkBibDB
      [ mkEntry "i1" "incollection"
        [ ("crossref", RawField "c1")
        , ("booktitle", LiteralField [Str "x1-booktitle"]) ]
      , mkEntry "c1" "collection"
        [ ("title", LiteralField [Str "c1-title"]) ]]
  ]

testsNormalize :: Test
testsNormalize = testGroup "normalize to biblatex model"
  [ testCase "book with no author is a collection" $
    fromBibTeX "" "@book{b, editor={Nobody}}"
    @?=
    mkBibDB [mkEntry "b" "collection"
      [("editor", AgentList [Agent [] [] [Str "Nobody"] []])]]
  , testCase "book with author and editor is still a book" $
    fromBibTeX "" "@book{b, author={A}, editor={E}}"
    @?=
    mkBibDB [mkEntry "b" "book"
      [ ("author", AgentList [Agent [] [] [Str "A"] []])
      , ("editor", AgentList [Agent [] [] [Str "E"] []])]]
  , testCase "field name \"address\" is renamed to \"location\"" $
    fromBibTeX "" "@book{b, author={A}, address={Berlin}}"
    @?=
    mkBibDB [mkEntry "b" "book"
      [ ("author", AgentList [Agent [] [] [Str "A"] []])
      , ("location", LiteralList [[Str "Berlin"]])]]
  , testCase "field name \"journal\" is renamed to \"journaltitle\"" $
    fromBibTeX "" "@article{a, journal={NN}}"
    @?=
    mkBibDB [mkEntry "a" "article"
      [("journaltitle", LiteralField [Str "NN"])]]
  ]

testsFormatter :: Test
testsFormatter = testGroup "bib formatter"
  [ testCase "single cite author" $
    getCiteAgents (snd bibEntry01)
    @?=
    [[Str "Büchner"]]
  , testCase "single cite year" $
    fmtCiteYear Nothing (snd bibEntry01)
    @?=
    [Str "1835"]
  , testCase "simple full citation" $
    fmtCiteFull Nothing (snd bibEntry01)
    @?=
    [ Str "Büchner", Str ",", Space, Str "Georg", Str ".", Space
    , Str "1835", Str ".", Space, FontStyle Emph [Str "Lenz"], Str "."]
  ]


-------------------- helpers

-- Parse the content of a bibfield to a list of inlines.
toInlines :: String -> [Inline]
toInlines = tex2inlines . readTeX "bibfield"

-- Create a BibDB from entries, wrapped in Either,
-- as returned by 'fromBibTeX'.
mkBibDB :: [(CiteKey, BibEntry)] -> Either String BibDB
mkBibDB = return . M.fromList

-- Create a \@book entry from a citekey and a list of literal fields.
mkBookEntry :: CiteKey -> [(BibFieldName, [Inline])] -> (CiteKey, BibEntry)
mkBookEntry key fs = (key, BibEntry "book"
  (M.fromList (map (fmap LiteralField) fs)))

-- Create a general entry from a citekey, an entry type name,
-- and a list of BibFields.
mkEntry :: CiteKey -> Text -> [(BibFieldName, BibFieldValue)] -> (CiteKey, BibEntry)
mkEntry key btype bfields = (key, BibEntry btype (M.fromList bfields))


-------------------- example data

bibFile01 :: Text
bibFile01 = "@book{büchner35, author = {Büchner, Georg}, title={Lenz}, year=1835}"

bibFile01a :: Text
bibFile01a = T.concat
  [ "@book{büchner35,"
  , " author = {Bü}#\"ch\" # {ner}#{,} #\n { Georg },"
  , " title={Le} # {nz}, year=1835}"]

bibEntry01 :: (CiteKey, BibEntry)
bibEntry01 = ("büchner35", BibEntry "book" $ M.fromList
  [ ("author", AgentList [Agent [Str "Georg"] [] [Str "Büchner"] []])
  , ("title", LiteralField [Str "Lenz"])
  , ("year", LiteralField [Str "1835"])])
