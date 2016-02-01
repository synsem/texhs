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
import qualified Data.Text as T
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
  , testCase "single entry with three authors" $
    let bibdb = M.fromList [("six", bibEntry06)]
        citemap = M.fromList [("six", 0)]
        citedb = M.fromList [("six", citeEntry06 0 "")]
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
  , testCase "bare citation of a single citekey without notes" $
    let citedb = M.fromList [("one", citeEntry01 0 "")]
        cit = MultiCite CiteBare [] [] [SingleCite [] [] ["one"]]
        title01 = mkCiteLinkTitle (citeEntry01 0 "")
    in fmtMultiCite citedb cit
       @?=
       [ Pointer "" (Just (ExternalResource [Str "Last"] "#bib-0" title01 "citation"))
       , Space
       , Pointer "" (Just (ExternalResource [Str "2000"] "#bib-0" title01 "citation")) ]
  , testCase "bare citation of a single citekey with postnote" $
    let citedb = M.fromList [("one", citeEntry01 0 "")]
        cit = MultiCite CiteBare [] [] [SingleCite [] [Str "19"] ["one"]]
        title01 = mkCiteLinkTitle (citeEntry01 0 "")
    in fmtMultiCite citedb cit
       @?=
       [ Pointer "" (Just (ExternalResource [Str "Last"] "#bib-0" title01 "citation"))
       , Space
       , Pointer "" (Just (ExternalResource [Str "2000"] "#bib-0" title01 "citation"))
       , Str ",", Space, Str "19" ]
  , testCase "paren citation of a single citekey without notes" $
    let citedb = M.fromList [("one", citeEntry01 0 "")]
        cit = MultiCite CiteParen [] [] [SingleCite [] [] ["one"]]
        title01 = mkCiteLinkTitle (citeEntry01 0 "")
    in fmtMultiCite citedb cit
       @?=
       [ Str "("
       , Pointer "" (Just (ExternalResource [Str "Last"] "#bib-0" title01 "citation"))
       , Space
       , Pointer "" (Just (ExternalResource [Str "2000"] "#bib-0" title01 "citation"))
       , Str ")" ]
  , testCase "paren citation of a single citekey with postnote" $
    let citedb = M.fromList [("one", citeEntry01 0 "")]
        cit = MultiCite CiteParen [] [] [SingleCite [] [Str "19"] ["one"]]
        title01 = mkCiteLinkTitle (citeEntry01 0 "")
    in fmtMultiCite citedb cit
       @?=
       [ Str "("
       , Pointer "" (Just (ExternalResource [Str "Last"] "#bib-0" title01 "citation"))
       , Space
       , Pointer "" (Just (ExternalResource [Str "2000"] "#bib-0" title01 "citation"))
       , Str ",", Space, Str "19", Str ")" ]
  , testCase "text citation of a single citekey without notes" $
    let citedb = M.fromList [("one", citeEntry01 0 "")]
        cit = MultiCite CiteText [] [] [SingleCite [] [] ["one"]]
        title01 = mkCiteLinkTitle (citeEntry01 0 "")
    in fmtMultiCite citedb cit
       @?=
       [ Pointer "" (Just (ExternalResource [Str "Last"] "#bib-0" title01 "citation"))
       , Space, Str "("
       , Pointer "" (Just (ExternalResource [Str "2000"] "#bib-0" title01 "citation"))
       , Str ")" ]
  , testCase "text citation of a single citekey with postnote" $
    let citedb = M.fromList [("one", citeEntry01 0 "")]
        cit = MultiCite CiteText [] [] [SingleCite [] [Str "19"] ["one"]]
        title01 = mkCiteLinkTitle (citeEntry01 0 "")
    in fmtMultiCite citedb cit
       @?=
       [ Pointer "" (Just (ExternalResource [Str "Last"] "#bib-0" title01 "citation"))
       , Space, Str "("
       , Pointer "" (Just (ExternalResource [Str "2000"] "#bib-0" title01 "citation"))
       , Str ",", Space, Str "19", Str ")" ]
  , testCase "bare citation of two citekeys with prenote" $
    let cit = MultiCite CiteBare [] [] [SingleCite [Str "see"] [] ["four", "seven"]]
        title04 = mkCiteLinkTitle (citeEntry04 12 "")
        title07 = mkCiteLinkTitle (citeEntry07 7 "a")
    in fmtMultiCite citeDB01 cit
       @?=
       [ Str "see", Space
       , Pointer "" (Just (ExternalResource [Str "Last1"] "#bib-12" title04 "citation"))
       , Space
       , Pointer "" (Just (ExternalResource [Str "2000"] "#bib-12" title04 "citation"))
       , Str ";", Space
       , Pointer "" (Just (ExternalResource [Str "Last"] "#bib-7" title07 "citation"))
       , Space
       , Pointer "" (Just (ExternalResource [Str "2000", Str "a"] "#bib-7" title07 "citation"))
       ]
  , testCase "conflate adjacent citations by same author" $
    let cit = MultiCite CiteText [] [] [SingleCite [Str "see"] [Str "88"] ["seven", "three"]]
        title03 = mkCiteLinkTitle (citeEntry03 3 "")
        title07 = mkCiteLinkTitle (citeEntry07 7 "a")
    in fmtMultiCite citeDB01 cit
       @?=
       [ Pointer "" (Just (ExternalResource [Str "Last"] "#bib-7" title07 "citation"))
       , Space, Str "(", Str "see", Space
       , Pointer "" (Just (ExternalResource [Str "2000", Str "a"] "#bib-7" title07 "citation"))
       , Str ",", Space
       , Pointer "" (Just (ExternalResource [Str "1999"] "#bib-3" title03 "citation"))
       , Str ",", Space, Str "88", Str ")"
       ]
  , testCase "conflate adjacent citations by same authorgroup" $
    let cit = MultiCite CiteParen [] [] [SingleCite [] [] ["six", "eight", "seven"]]
        title06 = mkCiteLinkTitle (citeEntry06 14 "")
        title07 = mkCiteLinkTitle (citeEntry07 7 "a")
        title08 = mkCiteLinkTitle (citeEntry08 15 "")
        authors06 = [ Str "Last1", Str ",", Space, Str "Last2", Space
                    , Str "&", Space, Str "Last3"]
    in fmtMultiCite citeDB01 cit
       @?=
       [ Str "("
       , Pointer "" (Just (ExternalResource authors06 "#bib-14" title06 "citation"))
       , Space
       , Pointer "" (Just (ExternalResource [Str "2000"] "#bib-14" title06 "citation"))
       , Str ",", Space
       , Pointer "" (Just (ExternalResource [Str "1999"] "#bib-15" title08 "citation"))
       , Str ";", Space
       , Pointer "" (Just (ExternalResource [Str "Last"] "#bib-7" title07 "citation"))
       , Space
       , Pointer "" (Just (ExternalResource [Str "2000", Str "a"] "#bib-7" title07 "citation"))
       , Str ")"
       ]
  , testCase "multicite with two singlecites and several notes" $
    let cit = MultiCite CiteText [Str "aa"] [Str "bb"]
          [ SingleCite [Str "cc"] [Str "dd"] ["three"]
          , SingleCite [Str "ee"] [Str "ff"] ["seven"]]
        title03 = mkCiteLinkTitle (citeEntry03 3 "")
        title07 = mkCiteLinkTitle (citeEntry07 7 "a")
    in fmtMultiCite citeDB01 cit
       @?=
       [ Str "aa", Space
       , Pointer "" (Just (ExternalResource [Str "Last"] "#bib-3" title03 "citation"))
       , Space, Str "(", Str "cc", Space
       , Pointer "" (Just (ExternalResource [Str "1999"] "#bib-3" title03 "citation"))
       , Str ",", Space, Str "dd", Str ")", Str ",", Space, Str "and", Space
       , Pointer "" (Just (ExternalResource [Str "Last"] "#bib-7" title07 "citation"))
       , Space, Str "(", Str "ee", Space
       , Pointer "" (Just (ExternalResource [Str "2000", Str "a"] "#bib-7" title07 "citation"))
       , Str ",", Space, Str "ff", Str ")", Str ",", Space, Str "bb"
       ]
  , testCase "multicite with three singlecites and no notes" $
    let cit = MultiCite CiteParen [] []
          [ SingleCite [] [] ["three"]
          , SingleCite [] [] ["four"]
          , SingleCite [] [] ["seven"]]
        title03 = mkCiteLinkTitle (citeEntry03 3 "")
        title04 = mkCiteLinkTitle (citeEntry04 12 "")
        title07 = mkCiteLinkTitle (citeEntry07 7 "a")
    in fmtMultiCite citeDB01 cit
       @?=
       [ Str "("
       , Pointer "" (Just (ExternalResource [Str "Last"] "#bib-3" title03 "citation"))
       , Space
       , Pointer "" (Just (ExternalResource [Str "1999"] "#bib-3" title03 "citation"))
       , Str ";", Space
       , Pointer "" (Just (ExternalResource [Str "Last1"] "#bib-12" title04 "citation"))
       , Space
       , Pointer "" (Just (ExternalResource [Str "2000"] "#bib-12" title04 "citation"))
       , Str ";", Space
       , Pointer "" (Just (ExternalResource [Str "Last"] "#bib-7" title07 "citation"))
       , Space
       , Pointer "" (Just (ExternalResource [Str "2000", Str "a"] "#bib-7" title07 "citation"))
       , Str ")"
       ]
  , testCase "cite author of a single citekey without notes" $
    let citedb = M.fromList [("one", citeEntry01 0 "")]
        cit = MultiCite CiteAuthor [] [] [SingleCite [] [] ["one"]]
        title01 = mkCiteLinkTitle (citeEntry01 0 "")
    in fmtMultiCite citedb cit
       @?=
       [Pointer "" (Just (ExternalResource [Str "Last"] "#bib-0" title01 "citation"))]
  , testCase "cite author of a single citekey with prenote and postnote" $
    let citedb = M.fromList [("one", citeEntry01 0 "")]
        cit = MultiCite CiteAuthor [] [] [SingleCite [Str "see"] [Str "sometime"] ["one"]]
        title01 = mkCiteLinkTitle (citeEntry01 0 "")
    in fmtMultiCite citedb cit
       @?=
       [ Str "see", Space
       , Pointer "" (Just (ExternalResource [Str "Last"] "#bib-0" title01 "citation"))
       , Str ",", Space, Str "sometime" ]
  , testCase "cite year of a single citekey without notes" $
    let citedb = M.fromList [("one", citeEntry01 0 "")]
        cit = MultiCite CiteYear [] [] [SingleCite [] [] ["one"]]
        title01 = mkCiteLinkTitle (citeEntry01 0 "")
    in fmtMultiCite citedb cit
       @?=
       [Pointer "" (Just (ExternalResource [Str "2000"] "#bib-0" title01 "citation"))]
  , testCase "cite year of a single citekey with (inner) prenote" $
    let citedb = M.fromList [("one", citeEntry01 0 "")]
        cit = MultiCite CiteYear [] [] [SingleCite [Str "written", Space, Str "in"] [] ["one"]]
        title01 = mkCiteLinkTitle (citeEntry01 0 "")
    in fmtMultiCite citedb cit
       @?=
       [ Str "written", Space, Str "in", Space
       , Pointer "" (Just (ExternalResource [Str "2000"] "#bib-0" title01 "citation")) ]
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
  , ("eight", bibEntry08)
  ]

citeDB01 :: CiteDB
citeDB01 = M.fromList
  [ ("one", citeEntry01 0 "b")
  , ("three", citeEntry03 3 "")
  , ("two", citeEntry02 4 "c")
  , ("seven", citeEntry07 7 "a")
  , ("four", citeEntry04 12 "")
  , ("six", citeEntry06 14 "")
  , ("eight", citeEntry08 15 "")
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

citeEntry06 :: Int -> String -> CiteEntry
citeEntry06 n extrayear =
  let year = Str "2000" : str extrayear
  in CiteEntry
     { citeAnchor = BibAnchor n
     , citeAgents = [[Str "Last1"], [Str "Last2"], [Str "Last3"]]
     , citeYear = year
     , citeFull = [ Str "Last1", Str ",", Space, Str "First1", Str ",", Space
                  , Str "First2", Space, Str "Last2", Space, Str "&", Space
                  , Str "First3", Space, Str "Last3", Str ".", Space ] ++ year ++
                  [ Str ".", Space, FontStyle Emph [Str "Title", Space, Str "six"]
                  , Str "."]
     }

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

-- entry with three authors
bibEntry08 :: BibEntry
bibEntry08 = BibEntry "book" $ M.fromList
  [ ("author", AgentList [ Agent [Str "First1"] [] [Str "Last1"] []
                         , Agent [Str "First2"] [] [Str "Last2"] []
                         , Agent [Str "First3"] [] [Str "Last3"] []
                         ])
  , ("year", LiteralField [Str "1999"])
  , ("title", LiteralField [Str "Title", Space, Str "eight"])
  ]

citeEntry08 :: Int -> String -> CiteEntry
citeEntry08 n extrayear =
  let year = Str "1999" : str extrayear
  in CiteEntry
     { citeAnchor = BibAnchor n
     , citeAgents = [[Str "Last1"], [Str "Last2"], [Str "Last3"]]
     , citeYear = year
     , citeFull = [ Str "Last1", Str ",", Space, Str "First1", Str ",", Space
                  , Str "First2", Space, Str "Last2", Space, Str "&", Space
                  , Str "First3", Space, Str "Last3", Str ".", Space ] ++ year ++
                  [ Str ".", Space, FontStyle Emph [Str "Title", Space, Str "eight"]
                  , Str "."]
     }



-------------------- helper

-- Convert string to inlines.
str :: String -> [Inline]
str xs = if null xs then [] else [Str xs]

-- Create expected value of the @title@ attribute.
mkCiteLinkTitle :: CiteEntry -> LinkTitle
mkCiteLinkTitle = T.pack . concatMap plain . citeFull
