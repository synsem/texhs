----------------------------------------------------------------------
-- texhs-test  : Test suite for the texhs library.
----------------------------------------------------------------------
-- Copyright   :  2015-2017 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
-- Maintainer  :  mschenner.dev@gmail.com
----------------------------------------------------------------------

module Main where

import Test.Framework (Test, defaultMain)

import qualified Text.BibSpec
import qualified Text.Bib.WriterSpec
import qualified Text.Doc.TypesSpec
import qualified Text.Doc.Reader.TeXSpec
import qualified Text.Doc.Filter.MultiFileSpec
import qualified Text.Doc.Writer.HtmlSpec
import qualified Text.Doc.Writer.XmlSpec
import qualified Text.TeX.LexerSpec
import qualified Text.TeX.ParserSpec
import qualified Text.TeXSpec


main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ Text.BibSpec.tests
  , Text.Bib.WriterSpec.tests
  , Text.Doc.TypesSpec.tests
  , Text.Doc.Reader.TeXSpec.tests
  , Text.Doc.Filter.MultiFileSpec.tests
  , Text.Doc.Writer.HtmlSpec.tests
  , Text.Doc.Writer.XmlSpec.tests
  , Text.TeX.LexerSpec.tests
  , Text.TeX.ParserSpec.tests
  , Text.TeXSpec.tests
  ]
