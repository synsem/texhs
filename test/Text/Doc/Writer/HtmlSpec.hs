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
      defaultMeta { metaTitle = [Str "No title"]
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
  , testCase "simple block quote" $
    blocks2html [QuotationBlock [Para [Str "one"]]]
    @?=
    "<blockquote><p>one</p></blockquote>"
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
  ]


-------------------- boilerplate constants

metaCharset :: Text
metaCharset = "<meta charset=\"utf-8\">"

metaViewport :: Text
metaViewport = "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"

metaGenerator :: Text
metaGenerator = "<meta name=\"generator\" content=\"texhs\">"
