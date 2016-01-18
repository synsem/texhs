{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
--
-- Module      :  Text.Doc.Writer.HtmlSpec
-- Copyright   :  2015-2016 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
-- Maintainer  :  mathias.schenner@langsci-press.org
--
-- Tests for the "Text.Doc.Writer.Html" module.
----------------------------------------------------------------------

module Text.Doc.Writer.HtmlSpec
  ( tests
  ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))
import qualified Data.Map.Strict as M
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT

import Text.Doc.Types
import Text.Doc.Writer.Html


-------------------- tests

tests :: Test
tests = testGroup "Text.Doc.Writer.HtmlSpec"
  [ testsDoc
  , testsBlocks
  , testsInlines
  ]

testsDoc :: Test
testsDoc = testGroup "documents"
  [ testCase "empty document" $
    doc2html (Doc defaultMeta [])
    @?=
    LT.concat [ "<!DOCTYPE HTML>\n<html><head>"
              , metaCharset
              , "<title></title>"
              , metaViewport
              , metaGenerator
              , "</head><body>"
              , "<h1></h1><h2></h2>"
              , "</body></html>"]
  , testCase "simple document" $
    doc2html (Doc
      defaultMeta { metaTitle = [Str "No", Space, Str "title"]
                  , metaAuthors = [[Str "Nobody"]]
                  , metaDate = [Str "2015-12-31"] }
      [Para [Str "hello", Space, Emph [Str "world"]]])
    @?=
    LT.concat [ "<!DOCTYPE HTML>\n<html><head>"
              , metaCharset
              , "<title>No title</title>"
              , metaViewport
              , metaGenerator
              , "</head><body>"
              , "<h1>No title</h1><h2>Nobody</h2>"
              , "<p>hello <em>world</em></p>"
              , "</body></html>"]
  , testCase "document with chapters and footnotes" $
    doc2html (Doc
      defaultMeta { metaTitle = [Str "No", Space, Str "title"]
                  , metaAuthors = [[Str "Nobody"]]
                  , metaNoteMap = M.fromList
                    [((1,1), [Para [Str "Footnote", Space, Str "one"]])
                    ,((3,1), [Para [Str "Footnote", Space, Str "two"]])
                    ,((3,2), [Para [Str "Footnote", Space, Str "three"]])
                    ,((3,3), [Para [Str "Footnote", Space, Str "four"]])]}
      [ Header 2 (SectionAnchor [0,1,0,0,0,0]) [Str "one"]
      , Para [ Str "One"
             , Note (NoteAnchor (1,1)) [Para [Str "Footnote", Space, Str "one"]]
             , Str "."]
      , Header 2 (SectionAnchor [0,2,0,0,0,0]) [Str "two"]
      , Para [ Str "No", Space, Str "footnotes", Space, Str "in", Space
             , Str "Chapter", Space, Str "two."]
      , Header 2 (SectionAnchor [0,3,0,0,0,0]) [Str "three"]
      , Para [ Str "Hello"
             , Note (NoteAnchor (3,1)) [Para [Str "Footnote", Space, Str "two"]]
             , Space, Str "world"
             , Note (NoteAnchor (3,2)) [Para [Str "Footnote", Space, Str "three"]]
             , Str "."]
      , Para [ Str "Hello"
             , Note (NoteAnchor (3,3)) [Para [Str "Footnote", Space, Str "four"]], Str "."]])
    @?=
    LT.concat [ "<!DOCTYPE HTML>\n<html><head>"
              , metaCharset
              , "<title>No title</title>"
              , metaViewport
              , metaGenerator
              , "</head><body>"
              , "<h1>No title</h1><h2>Nobody</h2>"
                -- body
              , "<h2 id=\"Pt0Ch1S0s0ss0p0\">one</h2>"
              , "<p>One"
              , "<a id=\"fn1chap1ref\" class=\"fnRef\" href=\"#fn1chap1\"><sup>1.1</sup></a>"
              , ".</p>"
              , "<h2 id=\"Pt0Ch2S0s0ss0p0\">two</h2>"
              , "<p>No footnotes in Chapter two.</p>"
              , "<h2 id=\"Pt0Ch3S0s0ss0p0\">three</h2>"
              , "<p>Hello"
              , "<a id=\"fn1chap3ref\" class=\"fnRef\" href=\"#fn1chap3\"><sup>3.1</sup></a>"
              , " world"
              , "<a id=\"fn2chap3ref\" class=\"fnRef\" href=\"#fn2chap3\"><sup>3.2</sup></a>"
              , ".</p><p>Hello"
              , "<a id=\"fn3chap3ref\" class=\"fnRef\" href=\"#fn3chap3\"><sup>3.3</sup></a>"
              , ".</p>"
                -- footnotes
              , "<h1 id=\"footnotes\">Footnotes</h1>"
              , "<h2 id=\"footnotesChap1\">Chapter 1</h2><ol>"
              , "<li id=\"fn1chap1\"><p>Footnote one</p><p><a href=\"#fn1chap1ref\">^</a></p></li>"
              , "</ol><h2 id=\"footnotesChap3\">Chapter 3</h2><ol>"
              , "<li id=\"fn1chap3\"><p>Footnote two</p><p><a href=\"#fn1chap3ref\">^</a></p></li>"
              , "<li id=\"fn2chap3\"><p>Footnote three</p><p><a href=\"#fn2chap3ref\">^</a></p></li>"
              , "<li id=\"fn3chap3\"><p>Footnote four</p><p><a href=\"#fn3chap3ref\">^</a></p></li>"
              , "</ol></body></html>"]
  ]

testsBlocks :: Test
testsBlocks = testGroup "blocks"
  [ testCase "single paragraph" $
    blocks2html [Para [Str "hello", Space, Emph [Str "world"]]]
    @?=
    "<p>hello <em>world</em></p>"
  , testCase "simple unordered list" $
    blocks2html [List UnorderedList
      [ [Para [Str "one",Space,Str "one"]]
      , [Para [Str "two",Space]]
      , [Para [Str "three"]]]]
    @?=
    LT.append "<ul><li><p>one one</p></li><li><p>two </p></li>"
              "<li><p>three</p></li></ul>"
  , testCase "simple ordered list" $
    blocks2html [List OrderedList
      [ [Para [Str "one"]]
      , [Para [Str "two"]]]]
    @?=
    LT.append "<ol><li><p>one</p></li>"
              "<li><p>two</p></li></ol>"
  , testCase "empty item list" $
    blocks2html [ListItemBlock []]
    @?=
    "<ol class=\"numbered-item-list\"></ol>"
  , testCase "item list with single item" $
    blocks2html [ListItemBlock [ListItem (ItemAnchor (0,[1]))
      [Para [Str "hello", Space, Str "world"]]]]
    @?=
    LT.concat [ "<ol class=\"numbered-item-list\">"
              , "<li id=\"item-0-1\" class=\"numbered-item\" value=\"1\">"
              , "<p>hello world</p></li></ol>"]
  , testCase "item list with multiple nested sublists" $
    blocks2html [ListItemBlock
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
    LT.concat [ "<ol class=\"numbered-item-list\">"
              , "<li id=\"item-0-1\" class=\"numbered-item\" value=\"1\">"
              , "<p>one</p>"
              , "<ol class=\"numbered-item-list\">"
              , "<li id=\"item-0-1-1\" class=\"numbered-item\" value=\"1\">"
              , "<p>one-one</p></li>"
              , "<li id=\"item-0-1-2\" class=\"numbered-item\" value=\"2\">"
              , "<p>one-two</p></li>"
              , "</ol></li>"
              , "<li id=\"item-0-2\" class=\"numbered-item\" value=\"2\">"
              , "<p>two</p>"
              , "<ol class=\"numbered-item-list\">"
              , "<li id=\"item-0-2-1\" class=\"numbered-item\" value=\"1\">"
              , "<p>two-one</p>"
              , "<ol class=\"numbered-item-list\">"
              , "<li id=\"item-0-2-1-1\" class=\"numbered-item\" value=\"1\">"
              , "<p>two-one-one</p></li>"
              , "<li id=\"item-0-2-1-2\" class=\"numbered-item\" value=\"2\">"
              , "<p>two-one-two</p></li>"
              , "</ol></li>"
              , "<li id=\"item-0-2-2\" class=\"numbered-item\" value=\"2\">"
              , "<p>two-two</p>"
              , "</li></ol></li></ol>"
              ]
  , testCase "simple block quote" $
    blocks2html [QuotationBlock [Para [Str "one"]]]
    @?=
    "<blockquote><p>one</p></blockquote>"
  , testCase "simple figure" $
    blocks2html [Figure (FigureAnchor (2,1)) "image.png" [Str "description"]]
    @?=
    LT.append "<figure id=\"figure1chap2\"><img src=\"image.png\">"
              "<figcaption>description</figcaption></figure>"
  , testCase "empty table" $
    blocks2html [Table (TableAnchor (2,1)) [Str "description"] []]
    @?=
    LT.append "<table id=\"table1chap2\"><caption>description</caption>"
              "<tbody></tbody></table>"
  , testCase "simple table" $
    blocks2html [Table (TableAnchor (2,1)) [Str "description"]
      [[SingleCell [Str "top-left"], SingleCell [Str "top-right"]]
      ,[SingleCell [Str "bottom-left"], SingleCell [Str "bottom-right"]]]]
    @?=
    LT.concat [ "<table id=\"table1chap2\">"
              , "<caption>description</caption>"
              , "<tbody>"
              , "<tr><td>top-left</td><td>top-right</td></tr>"
              , "<tr><td>bottom-left</td><td>bottom-right</td></tr>"
              , "</tbody></table>"]
  , testCase "table with multi-column cells" $
    blocks2html [Table (TableAnchor (3,4)) [Str "description"]
      [[SingleCell [Str "single", Space, Str "column"], MultiCell 2 [Str "two"]]
      ,[MultiCell 3 [Str "three", Space, Str "columns"]]
      ,[SingleCell [Str "1"], SingleCell [Str "2"], SingleCell [Str "3"]]]]
    @?=
    LT.concat [ "<table id=\"table4chap3\">"
              , "<caption>description</caption>"
              , "<tbody>"
              , "<tr><td>single column</td><td colspan=\"2\">two</td></tr>"
              , "<tr><td colspan=\"3\">three columns</td></tr>"
              , "<tr><td>1</td><td>2</td><td>3</td></tr>"
              , "</tbody></table>"]
  , testCase "empty simpletable" $
    blocks2html [SimpleTable []]
    @?=
    "<table><tbody></tbody></table>"
  , testCase "simple simpletable" $
    blocks2html [SimpleTable
      [[SingleCell [Str "one"], SingleCell [Str "one"]]
      ,[SingleCell [Str "two"], SingleCell [Str "two"]]]]
    @?=
    LT.concat [ "<table><tbody>"
              , "<tr><td>one</td><td>one</td></tr>"
              , "<tr><td>two</td><td>two</td></tr>"
              , "</tbody></table>"]
  , testCase "igt simpletable with translation line" $
    blocks2html [SimpleTable
      [ [SingleCell [Str "one"], SingleCell [Str "one"]]
      , [SingleCell [Str "two"], SingleCell [Str "two"]]
      , [MultiCell 2 [Str "translation", Space, Str "line"]]]]
    @?=
    LT.concat [ "<table><tbody>"
              , "<tr><td>one</td><td>one</td></tr>"
              , "<tr><td>two</td><td>two</td></tr>"
              , "<tr><td colspan=\"2\">translation line</td></tr>"
              , "</tbody></table>"]
  , testCase "misaligned igt simpletable with translation line" $
    blocks2html [SimpleTable
      [ [SingleCell [Str "one"]]
      , replicate 3 (SingleCell [Str "two"])
      , [MultiCell 3 [Str "translation", Space, Str "line"]]]]
    @?=
    LT.concat [ "<table><tbody>"
              , "<tr><td>one</td></tr>"
              , "<tr><td>two</td><td>two</td><td>two</td></tr>"
              , "<tr><td colspan=\"3\">translation line</td></tr>"
              , "</tbody></table>"]
  ]

testsInlines :: Test
testsInlines = testGroup "inlines"
  [ testCase "basic text" $
    inlines2html [Str "hello", Space, Str "world"]
    @?=
    "hello world"
  , testCase "emphasis" $
    inlines2html [Str "hello", Space, Emph [Str "world"]]
    @?=
    "hello <em>world</em>"
  , testCase "link to external resource" $
    inlines2html [Pointer "external" (Just (ExternalResource
      [Str "some", Space, Str "description"] "http://example.com/"))]
    @?=
    "<a href=\"http://example.com/\">some description</a>"
  , testCase "link to internal figure" $
    inlines2html [Str "Figure", Space, Pointer "internallabel"
      (Just (InternalResource (FigureAnchor (2,1))))]
    @?=
    "Figure <a href=\"#figure1chap2\">2.1</a>"
  , testCase "empty footnote (only mark)" $
    inlines2html [Note (NoteAnchor (2,8)) []]
    @?=
    "<a id=\"fn8chap2ref\" class=\"fnRef\" href=\"#fn8chap2\"><sup>2.8</sup></a>"
  , testCase "simple footnote (only mark)" $
    inlines2html [Note (NoteAnchor (1,2)) [Para [Str "hello"]]]
    @?=
    "<a id=\"fn2chap1ref\" class=\"fnRef\" href=\"#fn2chap1\"><sup>1.2</sup></a>"
  ]


-------------------- boilerplate constants

metaCharset :: Text
metaCharset = "<meta charset=\"utf-8\">"

metaViewport :: Text
metaViewport = "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"

metaGenerator :: Text
metaGenerator = "<meta name=\"generator\" content=\"texhs\">"
