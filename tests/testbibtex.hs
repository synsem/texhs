{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
-- Tests for the @Text.Bib@ module.
----------------------------------------------------------------------
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
-- Maintainer  :  mathias.schenner@langsci-press.org
----------------------------------------------------------------------

module Main where

import Data.List (intersperse)
import Data.Text (Text)
import System.Exit (ExitCode, exitSuccess, exitFailure)
import Test.HUnit (Test(..), Counts(..), test, (~:), (~=?), runTestTT)

import Text.Bib
import Text.Bib.Reader.BibTeX.Reference (parseAgents, parseList)
import Text.Doc.Types
import Text.Doc.Reader.TeX (tex2inlines)
import Text.TeX (readTeX)


-------------------- tests

testsNames :: Test
testsNames = TestLabel "name lists" $ test
  [ "First Last"
    ~: [Agent [Str "First"] [] [Str "Last"] []]
    ~=? (parseAgents (readTeX "" "First Last"))
  , "First von Last"
    ~: [Agent [Str "First"] [Str "von"] [Str "Last"] []]
    ~=? (parseAgents (readTeX "" "First von Last"))
  , "von Last"
    ~: [Agent [] [Str "von"] [Str "Last"] []]
    ~=? (parseAgents (readTeX "" "von Last"))
  , "von last"
    -- BibTeX warning: lower-case last name
    ~: [Agent [] [Str "von"] [Str "last"] []]
    ~=? (parseAgents (readTeX "" "von last"))
  , "Last"
    ~: [Agent [] [] [Str "Last"] []]
    ~=? (parseAgents (readTeX "" "Last"))
  , "last"
    -- BibTeX warning: lower-case last name
    ~: [Agent [] [] [Str "last"] []]
    ~=? (parseAgents (readTeX "" "last"))
  , "First von last"
    -- BibTeX warning: lower-case last name
    ~: [Agent [Str "First"] [Str "von"] [Str "last"] []]
    ~=? (parseAgents (readTeX "" "First von last"))
  , "First von von last"
    -- BibTeX warning: lower-case last name
    ~: [Agent [Str "First"] [Str "von", Space, Str "von"] [Str "last"] []]
    ~=? (parseAgents (readTeX "" "First von von last"))
  , "First von Last last Last"
    ~: [Agent [Str "First"] [Str "von"]
        [Str "Last", Space, Str "last", Space, Str "Last"] []]
    ~=? (parseAgents (readTeX "" "First von Last last Last"))
  , "First First Last"
    ~: [Agent [Str "First", Space, Str "First"] [] [Str "Last"] []]
    ~=? (parseAgents (readTeX "" "First First Last"))
  , "Last Last, First"
    ~: [Agent [Str "First"] [] [Str "Last", Space, Str "Last"] []]
    ~=? (parseAgents (readTeX "" "Last Last, First"))
  , "{Last, last}"
    -- Comma at brace level 1 is invisible during name-breaking.
    ~: [Agent [] [] [Str "Last,", Space, Str "last"] []]
    ~=? (parseAgents (readTeX "" "{Last, last}"))
  , "{Last{, }last}"
    -- Comma at brace level 2 is invisible during name-breaking.
    ~: [Agent [] [] [Str "Last,", Space, Str "last"] []]
    ~=? (parseAgents (readTeX "" "{Last{, }last}"))
  , "last, First"
    -- BibTeX warning: lower-case last name
    ~: [Agent [Str "First"] [] [Str "last"] []]
    ~=? (parseAgents (readTeX "" "last, First"))
  , "{von last}, First"
    -- BibTeX warning: lower-case last name
    ~: [Agent [Str "First"] [] [Str "von", Space, Str "last"] []]
    ~=? (parseAgents (readTeX "" "{von last}, First"))
  , "von Last Last, First first"
    ~: [Agent [Str "First", Space, Str "first"] [Str "von"]
        [Str "Last", Space, Str "Last"] []]
    ~=? (parseAgents (readTeX "" "von Last Last, First first"))
  , "von Last, Jr, First"
    ~: [Agent [Str "First"] [Str "von"] [Str "Last"] [Str "Jr"]]
    ~=? (parseAgents (readTeX "" "von Last, Jr, First"))
  , "von Last, Jr, First, First"
    -- BibTeX warning: too many commas
    ~: [Agent (intersperse Space (replicate 2 (Str "First")))
        [Str "von"] [Str "Last"] [Str "Jr"]]
    ~=? (parseAgents (readTeX "" "von Last, Jr, First, First"))
  , "von Last Last, Junior, First, First First, First"
    -- BibTeX warning: too many commas
    ~: [Agent (intersperse Space (replicate 4 (Str "First")))
        [Str "von"] [Str "Last", Space, Str "Last"] [Str "Junior"]]
    ~=? (parseAgents (readTeX ""
          "von Last Last, Junior, First, First First, First"))
  , "First Last and von Last, Jr, First, First"
    -- BibTeX warning: too many commas
    ~: [Agent [Str "First"] [] [Str "Last"] []
       , Agent (intersperse Space (replicate 2 (Str "First")))
         [Str "von"] [Str "Last"] [Str "Jr"]]
    ~=? (parseAgents (readTeX ""
          "First Last and von Last, Jr, First, First"))
  , "First Last and First Last"
    ~: replicate 2 (Agent [Str "First"] [] [Str "Last"] [])
    ~=? (parseAgents (readTeX "" "First Last and First Last"))
  , "First von Last and von Last, First"
    ~: replicate 2 (Agent [Str "First"] [Str "von"] [Str "Last"] [])
    ~=? (parseAgents (readTeX "" "First von Last and von Last, First"))
  , "one name, no commas, one 'von' part"
    ~: [Agent [Str "John"] [Str "von"] [Str "Neumann"] []]
    ~=? (parseAgents (readTeX "" "John von Neumann"))
  , "one name, one comma, one 'von' part"
    ~: [Agent [Str "John"] [Str "von"] [Str "Neumann"] []]
    ~=? (parseAgents (readTeX "" "von Neumann, John"))
  , "one name, one comma, two last names"
    ~: [Agent [Str "Per"] [] [Str "Brinch", Space, Str "Hansen"] []]
    ~=? (parseAgents (readTeX "" "Brinch Hansen, Per"))
  , "two names, no commas, no 'von' part"
    ~: [ Agent [Str "Alfred", Space, Str "North"] [] [Str "Whitehead"] []
       , Agent [Str "Bertrand"] [] [Str "Russell"] []]
    ~=? (parseAgents (readTeX "" "Alfred North Whitehead and Bertrand Russell"))
  , "one name, no commas, no 'von' part, with precomposed umlaut char"
    ~: [Agent [Str "Kurt"] [] [Str "Gödel"] []]
    ~=? (parseAgents (readTeX "" "Kurt Gödel"))
  , "one name, no commas, no 'von' part, with redundant whitespace"
    ~: [Agent [Str "Kurt"] [] [Str "Gödel"] []]
    ~=? (parseAgents (readTeX "" " Kurt  Gödel "))
  , "one name, one comma, no 'von' part, with redundant whitespace in braces"
    ~: [Agent [Str "Kurt"] [] [Str "Gödel"] []]
    ~=? (parseAgents (readTeX "" "{ Gödel }, Kurt"))
  , "one name, one comma, no 'von' part, with TeX-escaped special char"
    ~: [Agent [Str "Georg"] [] [Str "Bu\x0308\&chner"] []]
    ~=? (parseAgents (readTeX "" "B{\\\"u}chner, Georg"))
  , "one company name containing hidden 'and' (at brace level 1)"
    ~: [Agent [] [] (toInlines "{Nobody and Sons, Inc.}") []]
    ~=? (parseAgents (readTeX "" "{Nobody and Sons, Inc.}"))
  , "two names, combining one visible and one hidden 'and'"
    ~: [ Agent [] [] (toInlines "{National Aeronautics and Space Administration}") []
       , Agent [Str "John"] [] [Str "Doe"] []]
    ~=? (parseAgents (readTeX "" "{National Aeronautics and Space Administration} and Doe, John"))
  , "one name, no commas, multiple first, prefix and last names"
    ~: [Agent [Str "Charles", Space, Str "Louis", Space, Str "Xavier", Space, Str "Joseph"]
        [Str "de", Space, Str "la"] [Str "Valle\x0301\&e", Space, Str "Poussin"] []]
    ~=? (parseAgents (readTeX "" "Charles Louis Xavier Joseph de la Vall\\'ee Poussin"))
  ]

testsLiteralList :: Test
testsLiteralList = TestLabel "literal lists" $ test
  [ "zero items"
    ~: []
    ~=? (parseList (readTeX "" ""))
  , "one item"
    ~: [toInlines "one"]
    ~=? (parseList (readTeX "" "one"))
  , "two items"
    ~: map toInlines ["one", "two"]
    ~=? (parseList (readTeX "" "one and two"))
  , "three items"
    ~: map toInlines ["one", "two", "three"]
    ~=? (parseList (readTeX "" "one and two and three"))
  , "one item, with hidden 'and' in outer group"
    ~: [toInlines "{one and two}"]
    ~=? (parseList (readTeX "" "{one and two}"))
  , "one item, with hidden 'and' in inner group"
    ~: [toInlines "one {and} two"]
    ~=? (parseList (readTeX "" "one {and} two"))
  , "two items, combining visible and hidden 'and'"
    ~: map toInlines ["{one and one}", "two"]
    ~=? (parseList (readTeX "" "{one and one} and two"))
  , "delimiter 'and' must be surrounded by whitespace"
    ~: map toInlines ["land", "andy dandy"]
    ~=? (parseList (readTeX "" "land and andy dandy"))
  , "delimiter 'and' must be surrounded by whitespace"
    ~: [toInlines "dandy"]
    ~=? (parseList (readTeX "" "{d}and{y}"))
  , "delimiter 'and' must be surrounded by whitespace"
    ~: [toInlines "{d} and{y}"]
    ~=? (parseList (readTeX "" "{d} and{y}"))
  , "delimiter 'and' must be surrounded by whitespace"
    ~: [toInlines "{d}and {y}"]
    ~=? (parseList (readTeX "" "{d}and {y}"))
  , "delimiter 'and' must be surrounded by whitespace"
    ~: map toInlines ["{d}", "{y}"]
    ~=? (parseList (readTeX "" "{d} and {y}"))
  , "isolated 'and'"
    ~: []
    ~=? (parseList (readTeX "" "and"))
  , "double isolated 'and'"
    ~: []
    ~=? (parseList (readTeX "" "and and"))
  , "mixing visible and hidden 'and'"
    ~: map toInlines ["{and}", "{and and}", "{and}"]
    ~=? (parseList (readTeX "" "and {and} and {and and} and {and}"))
  , "empty left-hand side"
    ~: [toInlines "one"]
    ~=? (parseList (readTeX "" "and one"))
  , "empty right-hand side"
    ~: [toInlines "one"]
    ~=? (parseList (readTeX "" "one and"))
  , "white left-hand side"
    ~: [toInlines "one"]
    ~=? (parseList (readTeX "" " and one"))
  , "white right-hand side"
    ~: [toInlines "one"]
    ~=? (parseList (readTeX "" "one and "))
  , "'others' on right-hand side"
    -- We do not (yet) treat 'others' in a special way.
    ~: map toInlines ["one", "others"]
    ~=? (parseList (readTeX "" "one and others"))
  ]

testsEntry :: Test
testsEntry = TestLabel "parse bib entry" $ test
  [ "simple entry"
    ~: Right [bibEntry01]
    ~=? (fromBibTeX "" bibFile01)
  , "simple entry with hash-separated subfields"
    ~: Right [bibEntry01]
    ~=? (fromBibTeX "" bibFile01a)
  ]

testsFormatter :: Test
testsFormatter = TestLabel "bib formatter" $ test
  [ "single cite author"
    ~: [Str "Büchner"]
    ~=? (getCiteAgent bibEntry01)
  , "single cite year"
    ~: [Str "1835"]
    ~=? (getCiteYear bibEntry01)
  , "simple full citation"
    ~: [Str "Büchner", Space, Str "1835", Space, Str "Lenz", Str "."]
    ~=? (getCiteFull bibEntry01)
  ]

-- collect all tests
tests :: Test
tests = TestList
  [ testsNames
  , testsLiteralList
  , testsEntry
  , testsFormatter
  ]

-- run tests
main :: IO ExitCode
main = do
  counts <- runTestTT tests
  if errors counts == 0 && failures counts == 0
    then exitSuccess
    else exitFailure


-------------------- helpers

toInlines :: String -> [Inline]
toInlines = tex2inlines . readTeX "bibfield"


-------------------- example data

bibFile01 :: Text
bibFile01 = "@book{büchner35, author = {Büchner, Georg}, title={Lenz}, year=1835}"

bibFile01a :: Text
bibFile01a = "@book{büchner35,\
  \ author = {Bü}#\"ch\" # {ner}#{,} #\n { Georg },\
  \ title={Le} # {nz}, year=1835}"

bibEntry01 :: BibEntry
bibEntry01 = BibEntry "büchner35" "book"
  [("author", [Agent [Str "Georg"] [] [Str "Büchner"] []])]
  []
  [("citekey", [Str "büchner35"])
  ,("title", [Str "Lenz"])
  ,("year", [Str "1835"])]
