{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
--
-- Module      :  Text.Bib.WriterSpec
-- Copyright   :  2015-2016 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
-- Maintainer  :  mathias.schenner@langsci-press.org
--
-- Tests for the "Text.Bib.Writer" module.
----------------------------------------------------------------------

module Text.Bib.WriterSpec
  ( tests
  ) where

import qualified Data.Map.Strict as M
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

import Text.Bib.Types
import Text.Bib.Writer
import Text.Doc.Types


-------------------- tests

tests :: Test
tests = testGroup "Text.Bib.WriterSpec"
  [ testsResolve
  , testsFormatCite
  , testsFormatBib
  ]

testsResolve :: Test
testsResolve = testGroup "citation resolving"
  [ testCase "empty citemap" $
    resolveCitations M.empty M.empty
    @?=
    M.empty
  , testCase "single entry" $
    let bibdb = M.fromList [("one", bibEntry01)]
        citemap = M.fromList [("one", 0)]
        citedb = M.fromList [("one", citeEntry01 0 "")]
    in resolveCitations bibdb citemap
       @?=
       citedb
  , testCase "unambiguous citation despite ambiguous BibDB entries" $
    let citemap = M.fromList [("one", 0)]
        citedb = M.fromList [("one", citeEntry01 0 "")]
    in resolveCitations fullBibDB citemap
       @?=
       citedb
  , testCase "two entries with same author but different year" $
    let citemap = M.fromList [("one", 0), ("three", 3)]
        citedb = M.fromList [ ("one", citeEntry01 0 "")
                            , ("three", citeEntry03 3 "")]
    in resolveCitations fullBibDB citemap
       @?=
       citedb
  , testCase "two entries with same year but different author" $
    let citemap = M.fromList [("one", 0), ("four", 4)]
        citedb = M.fromList [ ("one", citeEntry01 0 "")
                            , ("four", citeEntry04 4 "")]
    in resolveCitations fullBibDB citemap
       @?=
       citedb
  , testCase "two entries with author-year ambiguity" $
    let citemap = M.fromList [("one", 0), ("two", 2)]
        citedb = M.fromList [ ("one", citeEntry01 0 "a")
                            , ("two", citeEntry02 2 "b")]
    in resolveCitations fullBibDB citemap
       @?=
       citedb
  , testCase "three entries with author-year ambiguity" $
    -- Note: Correct assignment of extrayear requires sorting by title:
    --   - 2000a : "A title"      (citeEntry07)
    --   - 2000b : "Title one"    (citeEntry01)
    --   - 2000c : "Title two"    (citeEntry02)
    let citemap = M.fromList [("one", 0), ("three", 3), ("two", 4), ("seven", 7)]
        citedb = M.fromList [ ("one", citeEntry01 0 "b")
                            , ("two", citeEntry02 4 "c")
                            , ("three", citeEntry03 3 "")
                            , ("seven", citeEntry07 7 "a")]
    in resolveCitations fullBibDB citemap
       @?=
       citedb
  ]

testsFormatCite :: Test
testsFormatCite = testGroup "citation formatting"
  [ testCase "invalid negative extra year" $
    fmtExtraYear (-1)
    @?=
    ""
  , testCase "extra year in ambiguous entry 0" $
    fmtExtraYear 0
    @?=
    "a"
  , testCase "extra year in ambiguous entry 26" $
    fmtExtraYear 26
    @?=
    "aa"
  , testCase "extra year in ambiguous entry 701" $
    fmtExtraYear 701
    @?=
    "zz"
  , testCase "extra year in ambiguous entry 702" $
    fmtExtraYear 702
    @?=
    "aaa"
  ]

testsFormatBib :: Test
testsFormatBib = testGroup "bibliography formatting"
  [ testCase "format agent: last, first" $
    fmtAgent LastFirst (Agent [Str "Georg"] [] [Str "Büchner"] [])
    @?=
    [Str "Büchner", Str ",", Space, Str "Georg"]
  , testCase "format agent: last, first von, jr" $
    fmtAgent LastFirst (Agent [Str "First"] [Str "von"] [Str "Last"] [Str "Jr."])
    @?=
    [ Str "Last", Str ",", Space, Str "First", Space, Str "von"
    , Str ",", Space, Str "Jr."]
  , testCase "format agent: first von last, jr" $
    fmtAgent FirstLast (Agent [Str "First"] [Str "von"] [Str "Last"] [Str "Jr."])
    @?=
    [ Str "First", Space, Str "von", Space, Str "Last"
    , Str ",", Space, Str "Jr."]
  , testCase "format agent: first von last" $
    fmtAgent FirstLast (Agent [Str "First"] [Str "von"] [Str "Last"] [])
    @?=
    [Str "First", Space, Str "von", Space, Str "Last"]
  , testCase "format single entry authors: last, first" $
    fmtBibFieldAuthors bibEntry01
    @?=
    [Str "Last", Str ",", Space, Str "First"]
  , testCase "format two entry authors" $
    fmtBibFieldAuthors bibEntry05
    @?=
    [ Str "Last1", Str ",", Space, Str "First1", Space, Str "&", Space
    , Str "First2", Space, Str "Last2"]
  , testCase "format three entry authors" $
    fmtBibFieldAuthors bibEntry06
    @?=
    [ Str "Last1", Str ",", Space, Str "First1", Str ",", Space
    , Str "First2", Space, Str "Last2", Space, Str "&", Space
    , Str "First3", Space, Str "Last3"]
  ]


-------------------- example data

fullBibDB :: BibDB
fullBibDB = M.fromList
  [ ("one", bibEntry01)
  , ("two", bibEntry02)
  , ("three", bibEntry03)
  , ("four", bibEntry04)
  , ("five", bibEntry05)
  , ("six", bibEntry06)
  , ("seven", bibEntry07)
  ]

bibEntry01 :: BibEntry
bibEntry01 = BibEntry "book" $ M.fromList
  [ ("author", AgentList [Agent [Str "First"] [] [Str "Last"] []])
  , ("year", LiteralField [Str "2000"])
  , ("title", LiteralField [Str "Title", Space, Str "one"])
  ]

citeEntry01 :: Int -> String -> CiteEntry
citeEntry01 n extrayear =
  let year = Str "2000" : str extrayear
  in CiteEntry
     { citeAnchor = BibAnchor n
     , citeAgents = [[Str "Last"]]
     , citeYear = year
     , citeFull = [ Str "Last", Str ",", Space, Str "First", Str ".", Space ] ++ year ++
                  [ Str ".", Space, FontStyle Emph [Str "Title", Space, Str "one"]
                  , Str "."]
     }

bibEntry02 :: BibEntry
bibEntry02 = BibEntry "book" $ M.fromList
  [ ("author", AgentList [Agent [Str "First"] [] [Str "Last"] []])
  , ("year", LiteralField [Str "2000"])
  , ("title", LiteralField [Str "Title", Space, Str "two"])
  ]

citeEntry02 :: Int -> String -> CiteEntry
citeEntry02 n extrayear =
  let year = Str "2000" : str extrayear
  in CiteEntry
     { citeAnchor = BibAnchor n
     , citeAgents = [[Str "Last"]]
     , citeYear = year
     , citeFull = [ Str "Last", Str ",", Space, Str "First", Str ".", Space] ++ year ++
                  [ Str ".", Space, FontStyle Emph [Str "Title", Space, Str "two"]
                  , Str "."]
     }

bibEntry03 :: BibEntry
bibEntry03 = BibEntry "book" $ M.fromList
  [ ("author", AgentList [Agent [Str "First"] [] [Str "Last"] []])
  , ("year", LiteralField [Str "1999"])
  , ("title", LiteralField [Str "Title", Space, Str "three"])
  ]

citeEntry03 :: Int -> String -> CiteEntry
citeEntry03 n extrayear =
  let year = Str "1999" : str extrayear
  in CiteEntry
     { citeAnchor = BibAnchor n
     , citeAgents = [[Str "Last"]]
     , citeYear = year
     , citeFull = [ Str "Last", Str ",", Space, Str "First", Str ".", Space] ++ year ++
                  [ Str ".", Space, FontStyle Emph [Str "Title", Space, Str "three"]
                  , Str "."]
     }

bibEntry04 :: BibEntry
bibEntry04 = BibEntry "book" $ M.fromList
  [ ("author", AgentList [Agent [Str "First1"] [] [Str "Last1"] []])
  , ("year", LiteralField [Str "2000"])
  , ("title", LiteralField [Str "Title", Space, Str "four"])
  ]

citeEntry04 :: Int -> String -> CiteEntry
citeEntry04 n extrayear =
  let year = Str "2000" : str extrayear
  in CiteEntry
     { citeAnchor = BibAnchor n
     , citeAgents = [[Str "Last1"]]
     , citeYear = year
     , citeFull = [ Str "Last1", Str ",", Space, Str "First1", Str ".", Space] ++ year ++
                  [ Str ".", Space, FontStyle Emph [Str "Title", Space, Str "four"]
                  , Str "."]
     }

-- entry with two authors
bibEntry05 :: BibEntry
bibEntry05 = BibEntry "book" $ M.fromList
  [ ("author", AgentList [ Agent [Str "First1"] [] [Str "Last1"] []
                         , Agent [Str "First2"] [] [Str "Last2"] []
                         ])
  , ("year", LiteralField [Str "2000"])
  , ("title", LiteralField [Str "Title", Space, Str "five"])
  ]

-- entry with three authors
bibEntry06 :: BibEntry
bibEntry06 = BibEntry "book" $ M.fromList
  [ ("author", AgentList [ Agent [Str "First1"] [] [Str "Last1"] []
                         , Agent [Str "First2"] [] [Str "Last2"] []
                         , Agent [Str "First3"] [] [Str "Last3"] []
                         ])
  , ("year", LiteralField [Str "2000"])
  , ("title", LiteralField [Str "Title", Space, Str "six"])
  ]

bibEntry07 :: BibEntry
bibEntry07 = BibEntry "book" $ M.fromList
  [ ("author", AgentList [Agent [Str "First"] [] [Str "Last"] []])
  , ("year", LiteralField [Str "2000"])
  , ("title", LiteralField [Str "A", Space, Str "title"])
  ]

citeEntry07 :: Int -> String -> CiteEntry
citeEntry07 n extrayear =
  let year = Str "2000" : str extrayear
  in CiteEntry
     { citeAnchor = BibAnchor n
     , citeAgents = [[Str "Last"]]
     , citeYear = year
     , citeFull = [ Str "Last", Str ",", Space, Str "First", Str ".", Space ] ++ year ++
                  [ Str ".", Space, FontStyle Emph [Str "A", Space, Str "title"]
                  , Str "."]
     }


-------------------- helper

-- Convert string to inlines.
str :: String -> [Inline]
str xs = if null xs then [] else [Str xs]
