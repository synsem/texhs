----------------------------------------------------------------------
-- texhs-test  : Test suite for the texhs library.
----------------------------------------------------------------------
-- Copyright   :  2015-2016 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
-- Maintainer  :  mathias.schenner@langsci-press.org
----------------------------------------------------------------------

module Main where

import Test.Framework (Test, defaultMain)

import qualified Text.BibSpec
import qualified Text.Doc.Reader.TeXSpec
import qualified Text.TeX.LexerSpec
import qualified Text.TeX.ParserSpec
import qualified Text.TeXSpec


main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ Text.BibSpec.tests
  , Text.Doc.Reader.TeXSpec.tests
  , Text.TeX.LexerSpec.tests
  , Text.TeX.ParserSpec.tests
  , Text.TeXSpec.tests
  ]
