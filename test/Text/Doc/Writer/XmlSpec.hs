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
              , "<titleStmt><title type=\"full\">"
              , "<title type=\"main\"></title></title></titleStmt>"
              , "<publicationStmt><p>Unknown</p></publicationStmt>"
              , "<sourceDesc><p>Born digital.</p></sourceDesc>"
              , "</fileDesc></teiHeader>"
              , "<text><front><titlePage>"
              , "<docTitle><titlePart type=\"main\"></titlePart></docTitle>"
              , "<byline></byline></titlePage></front>"
              , "<body></body>"
              , "<back></back></text></TEI>"]
  , testCase "simple document" $
    doc2xml (Doc
      defaultMeta { metaTitle = [Str "No", Space, Str "title"]
                  , metaSubTitle = [Str "No", Space, Str "subtitle"]
                  , metaAuthors = [[Str "Nobody"]]
                  , metaDate = [Str "2015-12-31"] }
      [ Header 2 (SectionAnchor (SectionInfo Mainmatter
          (SectionRegular (0,1,0,0,0,0,0))))
        [ Str "Chapter", Space, Str "one"]
      , Para [Str "hello", Space, FontStyle Emph [Str "world"]]])
    @?=
    LT.concat [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
              , "<TEI xmlns=\"http://www.tei-c.org/ns/1.0\">"
              , "<teiHeader><fileDesc>"
              , "<titleStmt><title type=\"full\">"
              , "<title type=\"main\">No title</title>"
              , "<title type=\"sub\">No subtitle</title></title>"
              , "<author>Nobody</author></titleStmt>"
              , "<publicationStmt><p>Unknown</p></publicationStmt>"
              , "<sourceDesc><p>Born digital.</p></sourceDesc>"
              , "</fileDesc></teiHeader>"
              , "<text><front><titlePage><docTitle>"
              , "<titlePart type=\"main\">No title</titlePart>"
              , "<titlePart type=\"sub\">No subtitle</titlePart></docTitle>"
              , "<byline><docAuthor>Nobody</docAuthor></byline></titlePage></front>"
              , "<body><div xml:id=\"sec-1\" type=\"chapter\">"
              , "<head>Chapter one</head>"
              , "<p>hello <emph>world</emph></p>"
              , "</div></body><back></back></text></TEI>"]
  ]

testsSections :: Test
testsSections = testGroup "sections"
  [ testCase "single paragraph in chapter" $
    sections2xml defaultMeta [Section 2
      (SectionAnchor (SectionInfo Mainmatter
        (SectionRegular (0,1,0,0,0,0,0))))
      [ Str "Chapter", Space, Str "one"]
      [ Para [Str "hello", Space, FontStyle Emph [Str "world"]]] []]
    @?=
    LT.concat [ "<div xml:id=\"sec-1\" type=\"chapter\">"
              , "<head>Chapter one</head>"
              , "<p>hello <emph>world</emph></p>"
              , "</div>"]
  ]

testsBlocks :: Test
testsBlocks = testGroup "blocks"
  [ testCase "single paragraph" $
    blocks2xml defaultMeta [Para [Str "hello", Space, FontStyle Emph [Str "world"]]]
    @?=
    "<p>hello <emph>world</emph></p>"
  , testCase "simple unordered list" $
    blocks2xml defaultMeta [List UnorderedList
      [ [Para [Str "one",Space,Str "one"]]
      , [Para [Str "two",Space]]
      , [Para [Str "three"]]]]
    @?=
    LT.append "<list type=\"unordered\"><item><p>one one</p></item>"
              "<item><p>two </p></item><item><p>three</p></item></list>"
  , testCase "simple ordered list" $
    blocks2xml defaultMeta [List OrderedList
      [ [Para [Str "one"]]
      , [Para [Str "two"]]]]
    @?=
    LT.append "<list type=\"ordered\"><item><p>one</p></item>"
              "<item><p>two</p></item></list>"
  , testCase "empty item list" $
    blocks2xml defaultMeta [ListItemBlock []]
    @?=
    "<list type=\"numbered-item-list\"></list>"
  , testCase "item list with single item" $
    blocks2xml defaultMeta [ListItemBlock [ListItem (ItemAnchor (0,[1]))
      [Para [Str "hello", Space, Str "world"]]]]
    @?=
    LT.concat [ "<list type=\"numbered-item-list\">"
              , "<item xml:id=\"item-0-1\" type=\"numbered-item\" n=\"1\">"
              , "<p>hello world</p></item></list>"]
  , testCase "item list with multiple nested sublists" $
    blocks2xml defaultMeta [ListItemBlock
      [ ListItem (ItemAnchor (0,[1]))
        [ Para [Str "one"], ListItemBlock
          [ ListItem (ItemAnchor (0,[1,1])) [Para [Str "one-one"]]
          , ListItem (ItemAnchor (0,[2,1])) [Para [Str "one-two"]]]]
      , ListItem (ItemAnchor (0,[2]))
        [ Para [Str "two"], ListItemBlock
          [ ListItem (ItemAnchor (0,[1,2]))
            [ Para [Str "two-one"], ListItemBlock
              [ ListItem (ItemAnchor (0,[1,1,2])) [Para [Str "two-one-one"]]
              , ListItem (ItemAnchor (0,[2,1,2])) [Para [Str "two-one-two"]]]]
          , ListItem (ItemAnchor (0,[2,2])) [Para [Str "two-two"]]]]]]
    @?=
    LT.concat [ "<list type=\"numbered-item-list\">"
              , "<item xml:id=\"item-0-1\" type=\"numbered-item\" n=\"1\">"
              , "<p>one</p>"
              , "<list type=\"numbered-item-list\">"
              , "<item xml:id=\"item-0-1-1\" type=\"numbered-item\" n=\"1\">"
              , "<p>one-one</p></item>"
              , "<item xml:id=\"item-0-1-2\" type=\"numbered-item\" n=\"2\">"
              , "<p>one-two</p></item>"
              , "</list></item>"
              , "<item xml:id=\"item-0-2\" type=\"numbered-item\" n=\"2\">"
              , "<p>two</p>"
              , "<list type=\"numbered-item-list\">"
              , "<item xml:id=\"item-0-2-1\" type=\"numbered-item\" n=\"1\">"
              , "<p>two-one</p>"
              , "<list type=\"numbered-item-list\">"
              , "<item xml:id=\"item-0-2-1-1\" type=\"numbered-item\" n=\"1\">"
              , "<p>two-one-one</p></item>"
              , "<item xml:id=\"item-0-2-1-2\" type=\"numbered-item\" n=\"2\">"
              , "<p>two-one-two</p></item>"
              , "</list></item>"
              , "<item xml:id=\"item-0-2-2\" type=\"numbered-item\" n=\"2\">"
              , "<p>two-two</p>"
              , "</item></list></item></list>"
              ]
  , testCase "simple block quote" $
    blocks2xml defaultMeta [QuotationBlock [Para [Str "one"]]]
    @?=
    "<quote><p>one</p></quote>"
  , testCase "simple figure" $
    blocks2xml defaultMeta [Figure (FigureAnchor (2,1)) "image.png" [Str "description"]]
    @?=
    LT.append "<figure xml:id=\"figure-2-1\"><graphic url=\"image.png\" />"
              "<head>description</head></figure>"
  , testCase "empty table" $
    blocks2xml defaultMeta [Table (TableAnchor (2,1)) [Str "description"] []]
    @?=
    "<table xml:id=\"table-2-1\"><head>description</head></table>"
  , testCase "simple table" $
    blocks2xml defaultMeta [Table (TableAnchor (2,1)) [Str "description"]
      [[SingleCell [Str "top-left"], SingleCell [Str "top-right"]]
      ,[SingleCell [Str "bottom-left"], SingleCell [Str "bottom-right"]]]]
    @?=
    LT.concat [ "<table xml:id=\"table-2-1\">"
              , "<head>description</head>"
              , "<row><cell>top-left</cell><cell>top-right</cell></row>"
              , "<row><cell>bottom-left</cell><cell>bottom-right</cell></row>"
              , "</table>"]
  , testCase "table with multi-column cells" $
    blocks2xml defaultMeta [Table (TableAnchor (3,4)) [Str "description"]
      [[SingleCell [Str "single", Space, Str "column"], MultiCell 2 [Str "two"]]
      ,[MultiCell 3 [Str "three", Space, Str "columns"]]
      ,[SingleCell [Str "1"], SingleCell [Str "2"], SingleCell [Str "3"]]]]
    @?=
    LT.concat [ "<table xml:id=\"table-3-4\">"
              , "<head>description</head>"
              , "<row><cell>single column</cell><cell cols=\"2\">two</cell></row>"
              , "<row><cell cols=\"3\">three columns</cell></row>"
              , "<row><cell>1</cell><cell>2</cell><cell>3</cell></row>"
              , "</table>"]
  , testCase "empty simpletable" $
    blocks2xml defaultMeta [SimpleTable []]
    @?=
    "<table></table>"
  , testCase "simple simpletable" $
    blocks2xml defaultMeta [SimpleTable
      [[SingleCell [Str "one"], SingleCell [Str "one"]]
      ,[SingleCell [Str "two"], SingleCell [Str "two"]]]]
    @?=
    LT.concat [ "<table>"
              , "<row><cell>one</cell><cell>one</cell></row>"
              , "<row><cell>two</cell><cell>two</cell></row>"
              , "</table>"]
  , testCase "igt simpletable with translation line" $
    blocks2xml defaultMeta [SimpleTable
      [ [SingleCell [Str "one"], SingleCell [Str "one"]]
      , [SingleCell [Str "two"], SingleCell [Str "two"]]
      , [MultiCell 2 [Str "translation", Space, Str "line"]]]]
    @?=
    LT.concat [ "<table>"
              , "<row><cell>one</cell><cell>one</cell></row>"
              , "<row><cell>two</cell><cell>two</cell></row>"
              , "<row><cell cols=\"2\">translation line</cell></row>"
              , "</table>"]
  , testCase "misaligned igt simpletable with translation line" $
    blocks2xml defaultMeta [SimpleTable
      [ [SingleCell [Str "one"]]
      , replicate 3 (SingleCell [Str "two"])
      , [MultiCell 3 [Str "translation", Space, Str "line"]]]]
    @?=
    LT.concat [ "<table>"
              , "<row><cell>one</cell></row>"
              , "<row><cell>two</cell><cell>two</cell><cell>two</cell></row>"
              , "<row><cell cols=\"3\">translation line</cell></row>"
              , "</table>"]
  ]

testsInlines :: Test
testsInlines = testGroup "inlines"
  [ testCase "basic text" $
    inlines2xml defaultMeta [Str "hello", Space, Str "world"]
    @?=
    "hello world"
  , testCase "emphasis" $
    inlines2xml defaultMeta [Str "hello", Space, FontStyle Emph [Str "world"]]
    @?=
    "hello <emph>world</emph>"
  , testCase "simple math with subscript and superscript" $
    inlines2xml defaultMeta [Math MathDisplay [Str "c",
      FontStyle Sub [Str "1"], FontStyle Sup [Str "2"]]]
    @?=
    LT.append "<formula>c<hi style=\"vertical-align: sub; font-size: smaller;\">1</hi>"
              "<hi style=\"vertical-align: super; font-size: smaller;\">2</hi></formula>"
  , testCase "link to external resource" $
    inlines2xml defaultMeta [Pointer "external" (Just (ExternalResource
      [Str "some", Space, Str "description"] "http://example.com/" "" ""))]
    @?=
    "<ref target=\"http://example.com/\">some description</ref>"
  , testCase "link to internal figure" $
    inlines2xml defaultMeta [Str "Figure", Space, Pointer "internallabel"
      (Just (InternalResourceAuto (FigureAnchor (2,1))))]
    @?=
    "Figure <ref target=\"#figure-2-1\">2.1</ref>"
  , testCase "empty footnote" $
    inlines2xml defaultMeta [Note (NoteAnchor (2,8)) []]
    @?=
    LT.append "<note xml:id=\"note-2-8\" type=\"footnote\" place=\"bottom\" n=\"2.8\">"
              "</note>"
  , testCase "simple footnote" $
    inlines2xml defaultMeta [Note (NoteAnchor (1,2)) [Para [Str "hello"]]]
    @?=
    LT.append "<note xml:id=\"note-1-2\" type=\"footnote\" place=\"bottom\" n=\"1.2\">"
              "<p>hello</p></note>"
  , testCase "multi-paragraph footnote" $
    inlines2xml defaultMeta [Note (NoteAnchor (1,2)) [Para [Str "one"], Para [Str "two"]]]
    @?=
    LT.append "<note xml:id=\"note-1-2\" type=\"footnote\" place=\"bottom\" n=\"1.2\">"
              "<p>one</p><p>two</p></note>"
  ]
