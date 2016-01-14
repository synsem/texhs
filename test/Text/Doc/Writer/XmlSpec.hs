{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
--
-- Module      :  Text.Doc.Writer.XmlSpec
-- Copyright   :  2015-2016 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
-- Maintainer  :  mathias.schenner@langsci-press.org
--
-- Tests for the "Text.Doc.Writer.Xml" module.
----------------------------------------------------------------------

module Text.Doc.Writer.XmlSpec
  ( tests
  ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))
import qualified Data.Text.Lazy as LT

import Text.Doc.Types
import Text.Doc.Section
import Text.Doc.Writer.Xml


-------------------- tests

tests :: Test
tests = testGroup "Text.Doc.Writer.XmlSpec"
  [ testsDoc
  , testsSections
  , testsBlocks
  , testsInlines
  ]

testsDoc :: Test
testsDoc = testGroup "documents"
  [ testCase "empty document" $
    doc2xml (Doc defaultMeta [])
    @?=
    LT.concat [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
              , "<TEI xmlns=\"http://www.tei-c.org/ns/1.0\">"
              , "<teiHeader><fileDesc>"
              , "<titleStmt><title></title></titleStmt>"
              , "<publicationStmt><p>Unknown</p></publicationStmt>"
              , "<sourceDesc><p>Born digital.</p></sourceDesc>"
              , "</fileDesc></teiHeader>"
              , "<text><front><titlePage>"
              , "<docTitle><titlePart type=\"main\"></titlePart></docTitle>"
              , "<byline></byline></titlePage></front>"
              , "<body></body>"
              , "<back /></text></TEI>"]
  , testCase "simple document" $
    doc2xml (Doc
      defaultMeta { metaTitle = [Str "No title"]
                  , metaAuthors = [[Str "Nobody"]]
                  , metaDate = [Str "2015-12-31"] }
      [ Header 3 (SectionAnchor [0,0,1,0,0,0]) [Str "Section", Space, Str "one"]
      , Para [Str "hello", Space, Emph [Str "world"]]])
    @?=
    LT.concat [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
              , "<TEI xmlns=\"http://www.tei-c.org/ns/1.0\">"
              , "<teiHeader><fileDesc>"
              , "<titleStmt><title>No title</title>"
              , "<author>Nobody</author></titleStmt>"
              , "<publicationStmt><p>Unknown</p></publicationStmt>"
              , "<sourceDesc><p>Born digital.</p></sourceDesc>"
              , "</fileDesc></teiHeader>"
              , "<text><front><titlePage>"
              , "<docTitle><titlePart type=\"main\">No title</titlePart></docTitle>"
              , "<byline><docAuthor>Nobody</docAuthor></byline></titlePage></front>"
              , "<body><div xml:id=\"Pt0Ch0S1s0ss0p0\" type=\"section\">"
              , "<head>Section one</head>"
              , "<p>hello <emph>world</emph></p>"
              , "</div></body><back /></text></TEI>"]
  ]

testsSections :: Test
testsSections = testGroup "sections"
  [ testCase "single paragraph in section with no subsections" $
    sections2xml [Section 3 (SectionAnchor [0,0,1,0,0,0])
      [ Str "Section", Space, Str "one"]
      [ Para [Str "hello", Space, Emph [Str "world"]]] []]
    @?=
    LT.concat [ "<div xml:id=\"Pt0Ch0S1s0ss0p0\" type=\"section\">"
              , "<head>Section one</head>"
              , "<p>hello <emph>world</emph></p>"
              , "</div>"]
  ]

testsBlocks :: Test
testsBlocks = testGroup "blocks"
  [ testCase "single paragraph" $
    blocks2xml [Para [Str "hello", Space, Emph [Str "world"]]]
    @?=
    "<p>hello <emph>world</emph></p>"
  , testCase "simple unordered list" $
    blocks2xml [List UnorderedList
      [ [Para [Str "one",Space,Str "one"]]
      , [Para [Str "two",Space]]
      , [Para [Str "three"]]]]
    @?=
    LT.append "<list type=\"unordered\"><item><p>one one</p></item>"
              "<item><p>two </p></item><item><p>three</p></item></list>"
  , testCase "simple ordered list" $
    blocks2xml [List OrderedList
      [ [Para [Str "one"]]
      , [Para [Str "two"]]]]
    @?=
    LT.append "<list type=\"ordered\"><item><p>one</p></item>"
              "<item><p>two</p></item></list>"
  , testCase "simple block quote" $
    blocks2xml [QuotationBlock [Para [Str "one"]]]
    @?=
    "<quote><p>one</p></quote>"
  , testCase "simple figure" $
    blocks2xml [Figure (FigureAnchor (2,1)) "image.png" [Str "description"]]
    @?=
    LT.append "<figure xml:id=\"figure1chap2\"><graphic url=\"image.png\" />"
              "<head>description</head></figure>"
  ]

testsInlines :: Test
testsInlines = testGroup "inlines"
  [ testCase "basic text" $
    inlines2xml [Str "hello", Space, Str "world"]
    @?=
    "hello world"
  , testCase "emphasis" $
    inlines2xml [Str "hello", Space, Emph [Str "world"]]
    @?=
    "hello <emph>world</emph>"
  , testCase "link to external resource" $
    inlines2xml [Pointer "external" (Just (ExternalResource
      [Str "some", Space, Str "description"] "http://example.com/"))]
    @?=
    "<ref target=\"http://example.com/\">some description</ref>"
  , testCase "link to internal figure" $
    inlines2xml [Str "Figure", Space, Pointer "internallabel"
      (Just (InternalResource (FigureAnchor (2,1))))]
    @?=
    "Figure <ref target=\"#figure1chap2\">2.1</ref>"
  ]
