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
    LT.concat [ "<!DOCTYPE HTML>\n"
              , "<html xmlns=\"http://www.w3.org/1999/xhtml\"><head>"
              , metaCharset
              , "<title></title>"
              , metaViewport
              , metaGenerator
              , "</head><body><header>"
              , "<h1 class=\"title\"></h1>"
              , "<h2 class=\"author\"></h2></header><main>"
              , "</main></body></html>"]
  , testCase "simple document" $
    doc2html docExample01
    @?=
    LT.concat [ "<!DOCTYPE HTML>\n"
              , "<html xmlns=\"http://www.w3.org/1999/xhtml\"><head>"
              , metaCharset
              , "<title>Some title</title>"
              , metaViewport
              , metaGenerator
              , "</head><body><header>"
              , "<h1 class=\"title\">Some title</h1>"
              , "<h1 class=\"subtitle\">Some subtitle</h1>"
              , "<h2 class=\"author\">Some Name</h2></header>"
              , "<nav id=\"toc\"><h2>Contents</h2><ul>"
              , "<li><a href=\"#sec-1\">1 one</a></li></ul></nav>"
              , "<main><section id=\"sec-1\"><h2>1 one</h2>"
              , "<p>hello <em>world</em></p>"
              , "</section></main></body></html>"]
  , testCase "simple document: XHTML1 version" $
    doc2html (toXhtml1 docExample01)
    @?=
    LT.concat [ doctypeXhtml1
              , "<html xmlns=\"http://www.w3.org/1999/xhtml\"><head>"
              , metaCharsetXhtml1
              , "<title>Some title</title>"
              , metaViewport
              , metaGenerator
              , "</head><body><div class=\"header\">"
              , "<h1 class=\"title\">Some title</h1>"
              , "<h1 class=\"subtitle\">Some subtitle</h1>"
              , "<h2 class=\"author\">Some Name</h2></div>"
              , "<div class=\"nav\" id=\"toc\"><h2>Contents</h2><ul>"
              , "<li><a href=\"#sec-1\">1 one</a></li></ul></div>"
              , "<div class=\"main\"><div class=\"section\" id=\"sec-1\"><h2>1 one</h2>"
              , "<p>hello <em>world</em></p>"
              , "</div></div></body></html>"]
  , testCase "document with chapters and footnotes" $
    doc2html docExample02
    @?=
    LT.concat [ "<!DOCTYPE HTML>\n"
              , "<html xmlns=\"http://www.w3.org/1999/xhtml\"><head>"
              , metaCharset
              , "<title>Some title</title>"
              , metaViewport
              , metaGenerator
              , "</head><body><header>"
              , "<h1 class=\"title\">Some title</h1>"
              , "<h2 class=\"author\">Some Name</h2></header>"
                -- nav
              , "<nav id=\"toc\"><h2>Contents</h2><ul>"
              , "<li><a href=\"#sec-1\">1 one</a></li>"
              , "<li><a href=\"#sec-2\">2 two</a><ul>"
              , "<li><a href=\"#sec-2-1\">2.1 two-sub1</a></li>"
              , "<li><a href=\"#sec-2-2\">2.2 two-sub2</a></li></ul></li>"
              , "<li><a href=\"#sec-3\">3 three</a></li>"
              , "<li><a href=\"#sec-back-unnumbered-1\">Notes</a><ul>"
              , "<li><a href=\"#sec-back-unnumbered-2\">Chapter 1</a></li>"
              , "<li><a href=\"#sec-back-unnumbered-3\">Chapter 3</a></li></ul></li></ul></nav>"
                -- content
              , "<main>"
              , "<section id=\"sec-1\"><h2>1 one</h2>"
              , "<p>One"
              , "<a id=\"note-1-1\" class=\"note-ref\" href=\"#notetext-1-1\"><sup>1.1</sup></a>"
              , ".</p></section>"
              , "<section id=\"sec-2\"><h2>2 two</h2>"
              , "<p>No footnotes in Chapter two.</p>"
              , "<section id=\"sec-2-1\"><h3>2.1 two-sub1</h3></section>"
              , "<section id=\"sec-2-2\"><h3>2.2 two-sub2</h3></section></section>"
              , "<section id=\"sec-3\"><h2>3 three</h2>"
              , "<p>Hello"
              , "<a id=\"note-3-1\" class=\"note-ref\" href=\"#notetext-3-1\"><sup>3.1</sup></a>"
              , " world"
              , "<a id=\"note-3-2\" class=\"note-ref\" href=\"#notetext-3-2\"><sup>3.2</sup></a>"
              , ".</p><p>Hello"
              , "<a id=\"note-3-3\" class=\"note-ref\" href=\"#notetext-3-3\"><sup>3.3</sup></a>"
              , ".</p></section>"
                -- footnotes
              , "<section id=\"sec-back-unnumbered-1\"><h2>Notes</h2>"
              , "<section id=\"sec-back-unnumbered-2\"><h3>Chapter 1</h3>"
              , "<ol class=\"notes\"><li id=\"notetext-1-1\"><p>Footnote one</p>"
              , "<p><a class=\"note-backref\" href=\"#note-1-1\">^</a></p></li></ol></section>"
              , "<section id=\"sec-back-unnumbered-3\"><h3>Chapter 3</h3>"
              , "<ol class=\"notes\"><li id=\"notetext-3-1\"><p>Footnote two</p>"
              , "<p><a class=\"note-backref\" href=\"#note-3-1\">^</a></p></li>"
              , "<li id=\"notetext-3-2\"><p>Footnote three</p>"
              , "<p><a class=\"note-backref\" href=\"#note-3-2\">^</a></p></li>"
              , "<li id=\"notetext-3-3\"><p>Footnote four</p>"
              , "<p><a class=\"note-backref\" href=\"#note-3-3\">^</a></p></li>"
              , "</ol></section></section></main></body></html>"]
  , testCase "document with chapters and footnotes: XHTML1 version" $
    doc2html (toXhtml1 docExample02)
    @?=
    LT.concat [ doctypeXhtml1
              , "<html xmlns=\"http://www.w3.org/1999/xhtml\"><head>"
              , metaCharsetXhtml1
              , "<title>Some title</title>"
              , metaViewport
              , metaGenerator
              , "</head><body><div class=\"header\">"
              , "<h1 class=\"title\">Some title</h1>"
              , "<h2 class=\"author\">Some Name</h2></div>"
                -- nav
              , "<div class=\"nav\" id=\"toc\"><h2>Contents</h2><ul>"
              , "<li><a href=\"#sec-1\">1 one</a></li>"
              , "<li><a href=\"#sec-2\">2 two</a><ul>"
              , "<li><a href=\"#sec-2-1\">2.1 two-sub1</a></li>"
              , "<li><a href=\"#sec-2-2\">2.2 two-sub2</a></li></ul></li>"
              , "<li><a href=\"#sec-3\">3 three</a></li>"
              , "<li><a href=\"#sec-back-unnumbered-1\">Notes</a><ul>"
              , "<li><a href=\"#sec-back-unnumbered-2\">Chapter 1</a></li>"
              , "<li><a href=\"#sec-back-unnumbered-3\">Chapter 3</a></li></ul></li></ul></div>"
                -- content
              , "<div class=\"main\">"
              , "<div class=\"section\" id=\"sec-1\"><h2>1 one</h2>"
              , "<p>One"
              , "<a id=\"note-1-1\" class=\"note-ref\" href=\"#notetext-1-1\"><sup>1.1</sup></a>"
              , ".</p></div>"
              , "<div class=\"section\" id=\"sec-2\"><h2>2 two</h2>"
              , "<p>No footnotes in Chapter two.</p>"
              , "<div class=\"section\" id=\"sec-2-1\"><h3>2.1 two-sub1</h3></div>"
              , "<div class=\"section\" id=\"sec-2-2\"><h3>2.2 two-sub2</h3></div></div>"
              , "<div class=\"section\" id=\"sec-3\"><h2>3 three</h2>"
              , "<p>Hello"
              , "<a id=\"note-3-1\" class=\"note-ref\" href=\"#notetext-3-1\"><sup>3.1</sup></a>"
              , " world"
              , "<a id=\"note-3-2\" class=\"note-ref\" href=\"#notetext-3-2\"><sup>3.2</sup></a>"
              , ".</p><p>Hello"
              , "<a id=\"note-3-3\" class=\"note-ref\" href=\"#notetext-3-3\"><sup>3.3</sup></a>"
              , ".</p></div>"
                -- footnotes
              , "<div class=\"section\" id=\"sec-back-unnumbered-1\"><h2>Notes</h2>"
              , "<div class=\"section\" id=\"sec-back-unnumbered-2\"><h3>Chapter 1</h3>"
              , "<ol class=\"notes\"><li id=\"notetext-1-1\"><p>Footnote one</p>"
              , "<p><a class=\"note-backref\" href=\"#note-1-1\">^</a></p></li></ol></div>"
              , "<div class=\"section\" id=\"sec-back-unnumbered-3\"><h3>Chapter 3</h3>"
              , "<ol class=\"notes\"><li id=\"notetext-3-1\"><p>Footnote two</p>"
              , "<p><a class=\"note-backref\" href=\"#note-3-1\">^</a></p></li>"
              , "<li id=\"notetext-3-2\"><p>Footnote three</p>"
              , "<p><a class=\"note-backref\" href=\"#note-3-2\">^</a></p></li>"
              , "<li id=\"notetext-3-3\"><p>Footnote four</p>"
              , "<p><a class=\"note-backref\" href=\"#note-3-3\">^</a></p></li>"
              , "</ol></div></div></div></body></html>"]
  ]

testsBlocks :: Test
testsBlocks = testGroup "blocks"
  [ testCase "single paragraph" $
    blocks2html defaultMeta [Para [Str "hello", Space, FontStyle Emph [Str "world"]]]
    @?=
    "<p>hello <em>world</em></p>"
  , testCase "simple unordered list" $
    blocks2html defaultMeta [List UnorderedList
      [ [Para [Str "one",Space,Str "one"]]
      , [Para [Str "two",Space]]
      , [Para [Str "three"]]]]
    @?=
    LT.append "<ul><li><p>one one</p></li><li><p>two </p></li>"
              "<li><p>three</p></li></ul>"
  , testCase "simple ordered list" $
    blocks2html defaultMeta [List OrderedList
      [ [Para [Str "one"]]
      , [Para [Str "two"]]]]
    @?=
    LT.append "<ol><li><p>one</p></li>"
              "<li><p>two</p></li></ol>"
  , testCase "empty item list" $
    blocks2html defaultMeta [AnchorList ItemList []]
    @?=
    "<ol class=\"numbered-item-list\"></ol>"
  , testCase "item list with single item" $
    blocks2html defaultMeta [AnchorList ItemList [ListItem (ItemAnchor (0,[1]))
      [Para [Str "hello", Space, Str "world"]]]]
    @?=
    LT.concat [ "<ol class=\"numbered-item-list\">"
              , "<li id=\"item-0-1\" class=\"numbered-item\" value=\"1\">"
              , "<p>hello world</p></li></ol>"]
  , testCase "item list with multiple nested sublists" $
    blocks2html defaultMeta [AnchorList ItemList
      [ ListItem (ItemAnchor (0,[1]))
        [ Para [Str "one"], AnchorList ItemList
          [ ListItem (ItemAnchor (0,[1,1])) [Para [Str "one-one"]]
          , ListItem (ItemAnchor (0,[2,1])) [Para [Str "one-two"]]]]
      , ListItem (ItemAnchor (0,[2]))
        [ Para [Str "two"], AnchorList ItemList
          [ ListItem (ItemAnchor (0,[1,2]))
            [ Para [Str "two-one"], AnchorList ItemList
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
  , testCase "simple note list" $
    blocks2html defaultMeta [AnchorList NoteList
      [ ListItem (NoteAnchor (2,4,NoteText)) [Para [Str "hello"]]]]
    @?=
    LT.append
      "<ol class=\"notes\"><li id=\"notetext-2-4\"><p>hello</p>"
      "<p><a class=\"note-backref\" href=\"#note-2-4\">^</a></p></li></ol>"
  , testCase "simple bib list" $
    blocks2html defaultMeta [BibList [ CiteEntry (BibAnchor 24)
      [[Str "Somebody"]] [Str "1999"] [Str "Full", Space, Str "entry."]]]
    @?=
    "<ol class=\"biblist\"><li id=\"bib-24\">Full entry.</li></ol>"
  , testCase "simple block quote" $
    blocks2html defaultMeta [QuotationBlock [Para [Str "one"]]]
    @?=
    "<blockquote><p>one</p></blockquote>"
  , testCase "simple figure" $
    blocks2html defaultMeta [Figure (FigureAnchor (2,1)) "image.png" [Str "description"]]
    @?=
    LT.append "<figure id=\"figure-2-1\"><img src=\"image.png\" />"
              "<figcaption>Figure 2.1: description</figcaption></figure>"
  , testCase "simple figure: XHTML1 version" $
    blocks2html (defaultMeta { metaWriterHtmlVersion = XHTML1 })
    [Figure (FigureAnchor (2,1)) "image.png" [Str "description"]]
    @?=
    LT.append "<div class=\"figure\" id=\"figure-2-1\"><img src=\"image.png\" />"
              "<p class=\"caption\">Figure 2.1: description</p></div>"
  , testCase "empty table" $
    blocks2html defaultMeta [Table (TableAnchor (2,1)) [Str "description"] []]
    @?=
    LT.append "<table id=\"table-2-1\"><caption>Table 2.1: description</caption>"
              "<tbody></tbody></table>"
  , testCase "simple table" $
    blocks2html defaultMeta [Table (TableAnchor (2,1)) [Str "description"]
      [[SingleCell [Str "top-left"], SingleCell [Str "top-right"]]
      ,[SingleCell [Str "bottom-left"], SingleCell [Str "bottom-right"]]]]
    @?=
    LT.concat [ "<table id=\"table-2-1\">"
              , "<caption>Table 2.1: description</caption>"
              , "<tbody>"
              , "<tr><td>top-left</td><td>top-right</td></tr>"
              , "<tr><td>bottom-left</td><td>bottom-right</td></tr>"
              , "</tbody></table>"]
  , testCase "table with multi-column cells" $
    blocks2html defaultMeta [Table (TableAnchor (3,4)) [Str "description"]
      [[SingleCell [Str "single", Space, Str "column"], MultiCell 2 [Str "two"]]
      ,[MultiCell 3 [Str "three", Space, Str "columns"]]
      ,[SingleCell [Str "1"], SingleCell [Str "2"], SingleCell [Str "3"]]]]
    @?=
    LT.concat [ "<table id=\"table-3-4\">"
              , "<caption>Table 3.4: description</caption>"
              , "<tbody>"
              , "<tr><td>single column</td><td colspan=\"2\">two</td></tr>"
              , "<tr><td colspan=\"3\">three columns</td></tr>"
              , "<tr><td>1</td><td>2</td><td>3</td></tr>"
              , "</tbody></table>"]
  , testCase "empty simpletable" $
    blocks2html defaultMeta [SimpleTable []]
    @?=
    "<table><tbody></tbody></table>"
  , testCase "simple simpletable" $
    blocks2html defaultMeta [SimpleTable
      [[SingleCell [Str "one"], SingleCell [Str "one"]]
      ,[SingleCell [Str "two"], SingleCell [Str "two"]]]]
    @?=
    LT.concat [ "<table><tbody>"
              , "<tr><td>one</td><td>one</td></tr>"
              , "<tr><td>two</td><td>two</td></tr>"
              , "</tbody></table>"]
  , testCase "igt simpletable with translation line" $
    blocks2html defaultMeta [SimpleTable
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
    blocks2html defaultMeta [SimpleTable
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
    inlines2html defaultMeta [Str "hello", Space, Str "world"]
    @?=
    "hello world"
  , testCase "emphasis" $
    inlines2html defaultMeta [Str "hello", Space, FontStyle Emph [Str "world"]]
    @?=
    "hello <em>world</em>"
  , testCase "simple math with subscript and superscript" $
    inlines2html defaultMeta [Math MathDisplay [Str "c",
      FontStyle Sub [Str "1"], FontStyle Sup [Str "2"]]]
    @?=
    "<span class=\"math\">c<sub>1</sub><sup>2</sup></span>"
  , testCase "link to external resource" $
    inlines2html defaultMeta [Pointer "external" (Just (ExternalResource
      [Str "some", Space, Str "description"] "http://example.com/" "" ""))]
    @?=
    "<a href=\"http://example.com/\">some description</a>"
  , testCase "link to internal figure" $
    inlines2html defaultMeta [Str "Figure", Space, Pointer "internallabel"
      (Just (InternalResourceAuto (FigureAnchor (2,1))))]
    @?=
    "Figure <a href=\"#figure-2-1\">2.1</a>"
  , testCase "link to internal figure in multifile document" $
    inlines2html (defaultMeta {
        metaAnchorFileMap = M.fromList [(FigureAnchor (2,1), 12)] })
      [Str "Figure", Space, Pointer "internallabel"
        (Just (InternalResourceAuto (FigureAnchor (2,1))))]
    @?=
    "Figure <a href=\"section-012.xhtml#figure-2-1\">2.1</a>"
  , testCase "empty footnote (only mark)" $
    inlines2html defaultMeta [Note (NoteAnchor (2,8,NoteMark)) []]
    @?=
    "<a id=\"note-2-8\" class=\"note-ref\" href=\"#notetext-2-8\"><sup>2.8</sup></a>"
  , testCase "simple footnote (only mark)" $
    inlines2html defaultMeta [Note (NoteAnchor (1,2,NoteMark)) [Para [Str "hello"]]]
    @?=
    "<a id=\"note-1-2\" class=\"note-ref\" href=\"#notetext-1-2\"><sup>1.2</sup></a>"
  ]


-------------------- helper

updateMeta :: (Meta -> Meta) -> Doc -> Doc
updateMeta f (Doc meta body) = Doc (f meta) body

toXhtml1 :: Doc -> Doc
toXhtml1 = updateMeta (\m -> m { metaWriterHtmlVersion = XHTML1 })


-------------------- example documents

docExample01 :: Doc
docExample01 = Doc
  defaultMeta { metaTitle = [Str "Some", Space, Str "title"]
              , metaSubTitle = [Str "Some", Space, Str "subtitle"]
              , metaAuthors = [[Str "Some Name"]]
              , metaDate = [Str "2015-12-31"] }
  [ Header 2
      (SectionAnchor (SectionInfo Mainmatter
        (SectionRegular (0,1,0,0,0,0,0))))
      [Str "one"]
  , Para [Str "hello", Space, FontStyle Emph [Str "world"]]]

docExample02 :: Doc
docExample02 = Doc
  defaultMeta { metaTitle = [Str "Some", Space, Str "title"]
              , metaAuthors = [[Str "Some Name"]]
              , metaNoteMap = M.fromList
                [((1,1), [Para [Str "Footnote", Space, Str "one"]])
                ,((3,1), [Para [Str "Footnote", Space, Str "two"]])
                ,((3,2), [Para [Str "Footnote", Space, Str "three"]])
                ,((3,3), [Para [Str "Footnote", Space, Str "four"]])]}
  [ Header 2
      (SectionAnchor (SectionInfo Mainmatter
        (SectionRegular (0,1,0,0,0,0,0))))
      [Str "one"]
  , Para [ Str "One"
         , Note (NoteAnchor (1,1,NoteMark)) [Para [Str "Footnote", Space, Str "one"]]
         , Str "."]
  , Header 2
      (SectionAnchor (SectionInfo Mainmatter
        (SectionRegular (0,2,0,0,0,0,0))))
      [Str "two"]
  , Para [ Str "No", Space, Str "footnotes", Space, Str "in", Space
         , Str "Chapter", Space, Str "two."]
  , Header 3
      (SectionAnchor (SectionInfo Mainmatter
        (SectionRegular (0,2,1,0,0,0,0))))
      [Str "two-sub1"]
  , Header 3
      (SectionAnchor (SectionInfo Mainmatter
        (SectionRegular (0,2,2,0,0,0,0))))
      [Str "two-sub2"]
  , Header 2
      (SectionAnchor (SectionInfo Mainmatter
        (SectionRegular (0,3,0,0,0,0,0))))
      [Str "three"]
  , Para [ Str "Hello"
         , Note (NoteAnchor (3,1,NoteMark)) [Para [Str "Footnote", Space, Str "two"]]
         , Space, Str "world"
         , Note (NoteAnchor (3,2,NoteMark)) [Para [Str "Footnote", Space, Str "three"]]
         , Str "."]
  , Para [ Str "Hello"
         , Note (NoteAnchor (3,3,NoteMark)) [Para [Str "Footnote", Space, Str "four"]]
         , Str "."]]


-------------------- boilerplate constants

doctypeXhtml1 :: Text
doctypeXhtml1 = LT.unlines
  [ "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\""
  , "    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"]

metaCharsetXhtml1 :: Text
metaCharsetXhtml1 =
  "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />"

metaCharset :: Text
metaCharset = "<meta charset=\"utf-8\" />"

metaViewport :: Text
metaViewport = "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\" />"

metaGenerator :: Text
metaGenerator = "<meta name=\"generator\" content=\"texhs\" />"
