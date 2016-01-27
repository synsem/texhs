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
  , testsFormat
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
        citedb = M.fromList [("one", citeEntry01 Nothing)]
    in resolveCitations bibdb citemap
       @?=
       citedb
  , testCase "unambiguous citation despite ambiguous BibDB entries" $
    let citemap = M.fromList [("one", 0)]
        citedb = M.fromList [("one", citeEntry01 Nothing)]
    in resolveCitations fullBibDB citemap
       @?=
       citedb
  , testCase "two entries with same author but different year" $
    let citemap = M.fromList [("one", 0), ("three", 3)]
        citedb = M.fromList [ ("one", citeEntry01 Nothing)
                            , ("three", citeEntry03 Nothing)]
    in resolveCitations fullBibDB citemap
       @?=
       citedb
  , testCase "two entries with same year but different author" $
    let citemap = M.fromList [("one", 0), ("four", 4)]
        citedb = M.fromList [ ("one", citeEntry01 Nothing)
                            , ("four", citeEntry04 Nothing)]
    in resolveCitations fullBibDB citemap
       @?=
       citedb
  , testCase "two entries with author-year ambiguity" $
    let citemap = M.fromList [("one", 0), ("two", 2)]
        citedb = M.fromList [ ("one", citeEntry01 (Just 0))
                            , ("two", citeEntry02 (Just 1))]
    in resolveCitations fullBibDB citemap
       @?=
       citedb
  ]

testsFormat :: Test
testsFormat = testGroup "citation formatting"
  [ testCase "extra year in unambiguous entry" $
    fmtExtraYear Nothing
    @?=
    ""
  , testCase "extra year in ambiguous entry 0" $
    fmtExtraYear (Just 0)
    @?=
    "a"
  , testCase "extra year in ambiguous entry 26" $
    fmtExtraYear (Just 26)
    @?=
    "aa"
  , testCase "extra year in ambiguous entry 701" $
    fmtExtraYear (Just 701)
    @?=
    "zz"
  , testCase "extra year in ambiguous entry 702" $
    fmtExtraYear (Just 702)
    @?=
    "aaa"
  ]


-------------------- example data

fullBibDB :: BibDB
fullBibDB = M.fromList
  [ ("one", bibEntry01)
  , ("two", bibEntry02)
  , ("three", bibEntry03)
  , ("four", bibEntry04)
  ]

bibEntry01 :: BibEntry
bibEntry01 = BibEntry "book" $ M.fromList
  [ ("author", AgentList [Agent [Str "First"] [] [Str "Last"] []])
  , ("year", LiteralField [Str "2000"])
  , ("title", LiteralField [Str "Title", Space, Str "one"])
  ]

citeEntry01 :: CiteUnique -> CiteEntry
citeEntry01 uniqcite = CiteEntry
  { citeAgents = [[Str "Last"]]
  , citeYear = [Str "2000"]
  , citeUnique = uniqcite
  , citeFull = [ Str "Last", Space, Str "2000", Space
               , Str "Title", Space, Str "one", Str "."]
  , citeAnchor = BibAnchor 0
  }

bibEntry02 :: BibEntry
bibEntry02 = BibEntry "book" $ M.fromList
  [ ("author", AgentList [Agent [Str "First"] [] [Str "Last"] []])
  , ("year", LiteralField [Str "2000"])
  , ("title", LiteralField [Str "Title", Space, Str "two"])
  ]

citeEntry02 :: CiteUnique -> CiteEntry
citeEntry02 uniqcite = CiteEntry
  { citeAgents = [[Str "Last"]]
  , citeYear = [Str "2000"]
  , citeUnique = uniqcite
  , citeFull = [ Str "Last", Space, Str "2000", Space
               , Str "Title", Space, Str "two", Str "."]
  , citeAnchor = BibAnchor 2
  }

bibEntry03 :: BibEntry
bibEntry03 = BibEntry "book" $ M.fromList
  [ ("author", AgentList [Agent [Str "First"] [] [Str "Last"] []])
  , ("year", LiteralField [Str "1999"])
  , ("title", LiteralField [Str "Title", Space, Str "three"])
  ]

citeEntry03 :: CiteUnique -> CiteEntry
citeEntry03 uniqcite = CiteEntry
  { citeAgents = [[Str "Last"]]
  , citeYear = [Str "1999"]
  , citeUnique = uniqcite
  , citeFull = [ Str "Last", Space, Str "1999", Space
               , Str "Title", Space, Str "three", Str "."]
  , citeAnchor = BibAnchor 3
  }

bibEntry04 :: BibEntry
bibEntry04 = BibEntry "book" $ M.fromList
  [ ("author", AgentList [Agent [Str "First1"] [] [Str "Last1"] []])
  , ("year", LiteralField [Str "2000"])
  , ("title", LiteralField [Str "Title", Space, Str "four"])
  ]

citeEntry04 :: CiteUnique -> CiteEntry
citeEntry04 uniqcite = CiteEntry
  { citeAgents = [[Str "Last1"]]
  , citeYear = [Str "2000"]
  , citeUnique = uniqcite
  , citeFull = [ Str "Last1", Space, Str "2000", Space
               , Str "Title", Space, Str "four", Str "."]
  , citeAnchor = BibAnchor 4
  }
