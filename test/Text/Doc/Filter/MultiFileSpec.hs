{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
--
-- Module      :  Text.Doc.Filter.MultiFileSpec
-- Copyright   :  2015-2017 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
-- Maintainer  :  mschenner.dev@gmail.com
--
-- Tests for the "Text.Doc.Filter.MultiFile" module.
----------------------------------------------------------------------

module Text.Doc.Filter.MultiFileSpec
  ( tests
  ) where

import qualified Data.Map.Strict as M
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

import Text.Doc.Types
import Text.Doc.Section
import Text.Doc.Filter.MultiFile


-------------------- tests

tests :: Test
tests = testGroup "Text.Doc.Filter.MultiFileSpec"
  [ testsSplit
  , testsAnchorFileMap
  ]

testsSplit :: Test
testsSplit = testGroup "split document"
  [ testCase "split empty document" $
    splitDoc 2 (SectionDoc defaultMeta [])
    @?=
    M.empty
  , testCase "split simple document at section level" $
    splitDoc 3 (SectionDoc defaultMeta
      [ Section 2 (mkSecAnc 0 1 0 0) [Str "a"] [Para [Str ".a."]]
        [ Section 3 (mkSecAnc 0 1 1 0) [Str "aa"] [Para [Str ".aa."]] []
        , Section 3 (mkSecAnc 0 1 2 0) [Str "ab"] [Para [Str ".ab."]] []
        ]
      , Section 2 (mkSecAnc 0 2 0 0) [Str "b"] [Para [Str ".b."]] []
      , Section 2 (mkSecAnc 0 3 0 0) [Str "c"] [Para [Str ".c."]]
        [ Section 3 (mkSecAnc 0 3 1 0) [Str "ca"] [Para [Str ".ca."]] []
        , Section 3 (mkSecAnc 0 3 2 0) [Str "cb"] [Para [Str ".cb."]] []
        ]
      ])
    @?=
    M.fromList
      [ (1, ContentFile $ Section 2 (mkSecAnc 0 1 0 0) [Str "a"] [Para [Str ".a."]] [])
      , (2, ContentFile $ Section 3 (mkSecAnc 0 1 1 0) [Str "aa"] [Para [Str ".aa."]] [])
      , (3, ContentFile $ Section 3 (mkSecAnc 0 1 2 0) [Str "ab"] [Para [Str ".ab."]] [])
      , (4, ContentFile $ Section 2 (mkSecAnc 0 2 0 0) [Str "b"] [Para [Str ".b."]] [])
      , (5, ContentFile $ Section 2 (mkSecAnc 0 3 0 0) [Str "c"] [Para [Str ".c."]] [])
      , (6, ContentFile $ Section 3 (mkSecAnc 0 3 1 0) [Str "ca"] [Para [Str ".ca."]] [])
      , (7, ContentFile $ Section 3 (mkSecAnc 0 3 2 0) [Str "cb"] [Para [Str ".cb."]] [])
      ]
  , testCase "split simple document without subsections at subsection level" $
    splitDoc 4 (SectionDoc defaultMeta
      [ Section 2 (mkSecAnc 0 1 0 0) [Str "a"] [Para [Str ".a."]]
        [ Section 3 (mkSecAnc 0 1 1 0) [Str "aa"] [Para [Str ".aa."]] []
        , Section 3 (mkSecAnc 0 1 2 0) [Str "ab"] [Para [Str ".ab."]] []
        ]
      , Section 2 (mkSecAnc 0 2 0 0) [Str "b"] [Para [Str ".b."]] []
      , Section 2 (mkSecAnc 0 3 0 0) [Str "c"] [Para [Str ".c."]]
        [ Section 3 (mkSecAnc 0 3 1 0) [Str "ca"] [Para [Str ".ca."]] []
        , Section 3 (mkSecAnc 0 3 2 0) [Str "cb"] [Para [Str ".cb."]] []
        ]
      ])
    @?=
    M.fromList
      [ (1, ContentFile $ Section 2 (mkSecAnc 0 1 0 0) [Str "a"] [Para [Str ".a."]] [])
      , (2, ContentFile $ Section 3 (mkSecAnc 0 1 1 0) [Str "aa"] [Para [Str ".aa."]] [])
      , (3, ContentFile $ Section 3 (mkSecAnc 0 1 2 0) [Str "ab"] [Para [Str ".ab."]] [])
      , (4, ContentFile $ Section 2 (mkSecAnc 0 2 0 0) [Str "b"] [Para [Str ".b."]] [])
      , (5, ContentFile $ Section 2 (mkSecAnc 0 3 0 0) [Str "c"] [Para [Str ".c."]] [])
      , (6, ContentFile $ Section 3 (mkSecAnc 0 3 1 0) [Str "ca"] [Para [Str ".ca."]] [])
      , (7, ContentFile $ Section 3 (mkSecAnc 0 3 2 0) [Str "cb"] [Para [Str ".cb."]] [])
      ]
  , testCase "split simple document at chapter level" $
    splitDoc 2 (SectionDoc defaultMeta
      [ Section 2 (mkSecAnc 0 1 0 0) [Str "a"] [Para [Str ".a."]]
        [ Section 3 (mkSecAnc 0 1 1 0) [Str "aa"] [Para [Str ".aa."]] []
        , Section 3 (mkSecAnc 0 1 2 0) [Str "ab"] [Para [Str ".ab."]] []
        ]
      , Section 2 (mkSecAnc 0 2 0 0) [Str "b"] [Para [Str ".b."]] []
      , Section 2 (mkSecAnc 0 3 0 0) [Str "c"] [Para [Str ".c."]]
        [ Section 3 (mkSecAnc 0 3 1 0) [Str "ca"] [Para [Str ".ca."]] []
        , Section 3 (mkSecAnc 0 3 2 0) [Str "cb"] [Para [Str ".cb."]] []
        ]
      ])
    @?=
    M.fromList
      [ (1, ContentFile $ Section 2 (mkSecAnc 0 1 0 0) [Str "a"] [Para [Str ".a."]]
              [ Section 3 (mkSecAnc 0 1 1 0) [Str "aa"] [Para [Str ".aa."]] []
              , Section 3 (mkSecAnc 0 1 2 0) [Str "ab"] [Para [Str ".ab."]] []])
      , (2, ContentFile $ Section 2 (mkSecAnc 0 2 0 0) [Str "b"] [Para [Str ".b."]] [])
      , (3, ContentFile $ Section 2 (mkSecAnc 0 3 0 0) [Str "c"] [Para [Str ".c."]]
            [ Section 3 (mkSecAnc 0 3 1 0) [Str "ca"] [Para [Str ".ca."]] []
            , Section 3 (mkSecAnc 0 3 2 0) [Str "cb"] [Para [Str ".cb."]] []])
      ]
  , testCase "split document with single part at part level: no split" $
    let examplePart =
          Section 1 (mkSecAnc 0 1 0 0) [Str "part1"] [Para [Str ".part1."]]
          [ Section 2 (mkSecAnc 0 1 0 0) [Str "a"] [Para [Str ".a."]]
            [ Section 3 (mkSecAnc 0 1 1 0) [Str "aa"] [Para [Str ".aa."]] []
            , Section 3 (mkSecAnc 0 1 2 0) [Str "ab"] [Para [Str ".ab."]] []
            ]
          , Section 2 (mkSecAnc 0 2 0 0) [Str "b"] [Para [Str ".b."]] []
          , Section 2 (mkSecAnc 0 3 0 0) [Str "c"] [Para [Str ".c."]]
            [ Section 3 (mkSecAnc 0 3 1 0) [Str "ca"] [Para [Str ".ca."]] []
            , Section 3 (mkSecAnc 0 3 2 0) [Str "cb"] [Para [Str ".cb."]] []
            ]
          ]
    in splitDoc 1 (SectionDoc defaultMeta [examplePart])
       @?=
       M.fromList [ (1, ContentFile examplePart) ]
  , testCase "split document without parts at part level: top-level split" $
    let exampleContent =
          [ Section 2 (mkSecAnc 0 1 0 0) [Str "a"] [Para [Str ".a."]]
            [ Section 3 (mkSecAnc 0 1 1 0) [Str "aa"] [Para [Str ".aa."]] []
            , Section 3 (mkSecAnc 0 1 2 0) [Str "ab"] [Para [Str ".ab."]] []
            ]
          , Section 2 (mkSecAnc 0 2 0 0) [Str "b"] [Para [Str ".b."]] []
          , Section 2 (mkSecAnc 0 3 0 0) [Str "c"] [Para [Str ".c."]]
            [ Section 3 (mkSecAnc 0 3 1 0) [Str "ca"] [Para [Str ".ca."]] []
            , Section 3 (mkSecAnc 0 3 2 0) [Str "cb"] [Para [Str ".cb."]] []
            ]
          ]
    in splitDoc 1 (SectionDoc defaultMeta exampleContent)
       @?=
       M.fromList (zip [1..] (map ContentFile exampleContent))
  ]

testsAnchorFileMap :: Test
testsAnchorFileMap = testGroup "anchor file map"
  [ testCase "empty map" $
    mkAnchorFileMap M.empty
    @?=
    M.empty
  , testCase "split simple document at section level" $
    mkAnchorFileMap (M.fromList
      [ (1, ContentFile $ Section 2 (mkSecAnc 0 1 0 0) [Str "a"]
            [Para [Str ".a."]] [])
      , (2, ContentFile $ Section 3 (mkSecAnc 0 1 1 0) [Str "aa"]
            [ Figure (FigureAnchor (0, 1)) 0 []
            , AnchorList ItemList [ListItem (ItemAnchor (0, [1])) []]] [])
      , (3, ContentFile $ Section 3 (mkSecAnc 0 1 2 0) [Str "ab"]
            [Para [Str ".ab."]] [])
      , (4, ContentFile $ Section 2 (mkSecAnc 0 2 0 0) [Str "b"]
            [ Para [Str ".b."]
              -- note: pointer to anchor should not be extracted
            , Para [ Pointer "" (Just (InternalResource [] (BibAnchor 0) "" ""))
                   , Note (NoteAnchor (0, 1, NoteMark)) []]
            , Para [Str ".b."]] [])
      , (5, ContentFile $ Section 2 (mkSecAnc 0 3 0 0) [Str "c"]
            [Para [Str ".c."]] [])
      , (6, ContentFile $ Section 3 (mkSecAnc 0 3 1 0) [Str "ca"]
            [Para [Str ".ca."], Table (TableAnchor (0, 1)) [] []] [])
      , (7, ContentFile $ Section 3 (mkSecAnc 0 3 2 0) [Str "cb"]
            [Para [Str ".cb."]] [])
      ])
    @?=
    M.fromList
      [ (mkSecAnc 0 1 0 0, 1)
      , (mkSecAnc 0 1 1 0, 2)
      , (mkSecAnc 0 1 2 0, 3)
      , (mkSecAnc 0 2 0 0, 4)
      , (mkSecAnc 0 3 0 0, 5)
      , (mkSecAnc 0 3 1 0, 6)
      , (mkSecAnc 0 3 2 0, 7)
      , (FigureAnchor (0, 1), 2)
      , (ItemAnchor (0, [1]), 2)
      , (NoteAnchor (0, 1, NoteMark), 4)
      , (TableAnchor (0, 1), 6)
      ]
  ]


-------------------- helpers

-- Create a section anchor for a numbered mainmatter heading
mkSecAnc :: Int -> Int -> Int -> Int -> InternalAnchor
mkSecAnc pt ch se sb = SectionAnchor $
  SectionInfo Mainmatter (SectionRegular (pt,ch,se,sb,0,0,0))
