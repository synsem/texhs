{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
--
-- Module      :  Text.Doc.TypesSpec
-- Copyright   :  2015-2016 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
-- Maintainer  :  mathias.schenner@langsci-press.org
--
-- Tests for the "Text.Doc.Types" module.
----------------------------------------------------------------------

module Text.Doc.TypesSpec
  ( tests
  ) where

import qualified Data.Map.Strict as M
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

import Text.Doc.Types


-------------------- tests

tests :: Test
tests = testGroup "Text.Doc.TypesSpec"
  [ testsAnchors
  ]


testsAnchors :: Test
testsAnchors = testGroup "anchors"
  [ testCase "internalAnchorID for section" $
    internalAnchorID (SectionAnchor $
      SectionInfo Mainmatter (SectionRegular (1,2,3,4,0,0,0)))
    @?=
    "sec-2-3-4"
  , testCase "anchorTarget for section in single-file document" $
    anchorTarget M.empty (InternalResourceAuto $ SectionAnchor $
      SectionInfo Mainmatter (SectionRegular (1,2,3,4,0,0,0)))
    @?=
    "#sec-2-3-4"
  , testCase "anchorTarget for section in multi-file document" $
    let anchor = SectionAnchor (SectionInfo Mainmatter (SectionRegular (1,2,3,4,0,0,0)))
        anchorFileMap = M.fromList [ (anchor, 22) ]
    in anchorTarget anchorFileMap (InternalResourceAuto anchor)
    @?=
    "section-022.xhtml#sec-2-3-4"
  , testCase "internalAnchorID for figure" $
    internalAnchorID (FigureAnchor (2, 4))
    @?=
    "figure-2-4"
  , testCase "anchorTarget for figure in single-file document" $
    anchorTarget M.empty (InternalResourceAuto (FigureAnchor (2, 4)))
    @?=
    "#figure-2-4"
  , testCase "anchorTarget for figure in multi-file document" $
    let anchor = FigureAnchor (2, 4)
        anchorFileMap = M.fromList [ (anchor, 7) ]
    in anchorTarget anchorFileMap (InternalResourceAuto anchor)
    @?=
    "section-007.xhtml#figure-2-4"
  ]
