{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
--
-- Module      :  Text.Doc.Reader.TeXSpec
-- Copyright   :  2015-2016 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
-- Maintainer  :  mathias.schenner@langsci-press.org
--
-- Tests for the "Text.Doc.Reader.TeX" module.
----------------------------------------------------------------------

module Text.Doc.Reader.TeXSpec
  ( tests
  ) where

#if MIN_VERSION_base(4,8,0)
-- Prelude exports all required operators from Control.Applicative
#else
import Control.Applicative ((<$>), (<*>), (<*), (*>))
#endif
import qualified Data.Map.Strict as M
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

import Text.TeX.Context.Types
import Text.TeX.Context.Walk
import Text.TeX.Parser.Types
import Text.Doc.Types
import Text.Doc.Reader.TeX


-------------------- tests

tests :: Test
tests = testGroup "Text.Doc.Reader.TeXSpec"
  [ testsBasic
  , testsBlocks
  , testsInlines
  , testsCites
  , testsMath
  , testsSections
  , testsLists
  , testsListItems
  , testsInterlinear
  , testsFigures
  , testsTables
  , testsFootnotes
  , testsCrossrefs
  , testsHyperref
  , testsWhitespace
  ]

testsBasic :: Test
testsBasic = testGroup "basic traversals"
  [ testCase "item" $
    runParser item example1
    @?=
    Right (Plain "hello")
  , testCase "chained item" $
    runParser (item >> item >> item) example1
    @?=
    Right (Plain "world!")
  , testCase "failing chained item" $
    runParser (item >> item >> item >> item) example1
    @?=
    Left [EndOfGroup]
  , testCase "satisfy" $
    runParser (satisfy isPlain) example1
    @?=
    Right (Plain "hello")
  , testCase "failing satisfy" $
    runParser (item >> satisfy isPlain) example1
    @?=
    Left [Unexpected White]
  , testCase "walk into group" $
    runParser (goDown >> item) example2
    @?=
    Right (Plain "hello")
  , testCase "failing item inside of group" $
    runParser (goDown >> item >> item) example2
    @?=
    Left [EndOfGroup]
  , testCase "enter and leave group" $
    runParser (goDown >> item >> goUp >> item) example2
    @?=
    Right White
  , testCase "flatten groups" $
    runParser inlines example3
    @?=
    Right [FontStyle Emph [Str "hello"], Space, Str "world", Str "!"]
  ]

testsBlocks :: Test
testsBlocks = testGroup "block elements"
  [ testCase "single section header" $
    runParser block
      [Command "section" [OblArg [Plain "one"]]]
    @?=
    Right (Header 3
      (SectionAnchor (SectionInfo Frontmatter
        (SectionRegular (0,0,1,0,0,0,0))))
      [Str "one"])
  , testCase "single paragraph" $
    runParser block [Plain "hello"]
    @?=
    Right (Para [Str "hello"])
  , testCase "simple block quote" $
    runParser (blocks <* eof)
      [Group "quotation" [] [Plain "one"]]
    @?=
    Right [QuotationBlock [Para [Str "one"]]]
  , testCase "block quote with leading and trailing par breaks" $
    runParser (blocks <* eof)
      [Group "quotation" [] [Par, Plain "one", Par], Par]
    @?=
    Right [QuotationBlock [Para [Str "one"]]]
  , testCase "adjacent inline groups form a single paragraph" $
    runParser (blocks <* eof)
      [ Plain "one", White
      , Group "" [] [Plain "two", White]
      , Group "" [] [Plain "three"]
      , Par
      , Plain "four"]
    @?=
    Right [ Para [Str "one", Space, Str "two", Space, Str "three"]
          , Para [Str "four"]]
  , testCase "adjacent inline groups form a single paragraph" $
    runParser (blocks <* eof)
      [ Group "" [] [Plain "one", White]
      , Group "" [] [Plain "two", White]
      , Group "" [] [Plain "three"]
      , Par
      , Group "" [] [Plain "four"]]
    @?=
    Right [ Para [Str "one", Space, Str "two", Space, Str "three"]
          , Para [Str "four"]]
  ]

testsInlines :: Test
testsInlines = testGroup "inline elements"
  [ testCase "simple emph" $
    runParser emph example4
    @?=
    Right (FontStyle Emph [Str "hello"])
  , testCase "emph with inner space" $
    runParser emph example5
    @?=
    Right (FontStyle Emph [Str "one",Space,Str "two"])
  , testCase "return to parent after emph" $
    runParser (emph *> space *> item) example4
    @?=
    Right (Plain "world")
  , testCase "em with inner space" $
    runParser em example6
    @?=
    Right (FontStyle Emph [Str "one",Space,Str "two"])
  , testCase "failing em" $
    runParser em example7
    @?=
    Left [Unexpected (Group "" []
      [Command "em" [], Plain "one", White, Plain "two"])]
  , testCase "nested em" $
    runParser (optNested em) example7
    @?=
    Right (FontStyle Emph [Str "one",Space,Str "two"])
  , testCase "combining nested em with parent inlines" $
    runParser ((:) <$> optNested em <*> inlines) example7
    @?=
    Right [FontStyle Emph [Str "one",Space,Str "two"],Space,Str "three"]
  , testCase "rm between em font switches" $
    runParser inlines example8
    @?=
    Right [FontStyle Emph [Str "one", FontStyle Normal
      [Str "two", FontStyle Emph [Str "three"]]]]
  , testCase "adjacent inlines in groups" $
    runParser (inlines <* eof)
      [ Group "" [] [Plain "one", White]
      , Group "" [] [Plain "two", White]
      , Group "" [] [Plain "three"]]
    @?=
    Right [Str "one", Space, Str "two", Space, Str "three"]
  ]

testsCites :: Test
testsCites = testGroup "citations"
  [ testCase "empty citation" $
    runParser (inlines <* eof)
      [Command "cite" [OblArg []]]
    @?=
    Right [Citation (MultiCite CiteBare [] []
      [SingleCite [] [] []]) Nothing]
  , testCase "simple citation" $
    runParser (inlines <* eof)
      [Command "cite" [OblArg [Plain "one"]]]
    @?=
    Right [Citation (MultiCite CiteBare [] []
      [SingleCite [] [] ["one"]]) Nothing]
  , testCase "citekey with space and underscore" $
    runParser (inlines <* eof)
      [Command "cite" [OblArg
        [Plain "a", SubScript [Plain "b"], Plain "2", White, Plain "c"]]]
    @?=
    Right [Citation (MultiCite CiteBare [] []
      [SingleCite [] [] ["a_b2 c"]]) Nothing]
  , testCase "nocitekey with space and underscore" $
    runParser (inlines <* eof)
      [Command "nocite" [OblArg
        [Plain "a", SubScript [Plain "b"], Plain "2", White, Plain "c"]]]
    @?=
    Right []
  , testCase "single citation with two keys" $
    runParser (inlines <* eof)
      [Command "cite" [OblArg [Plain "one,", White, Plain "two"]]]
    @?=
    Right [Citation (MultiCite CiteBare [] []
      [SingleCite [] [] ["one", "two"]]) Nothing]
  , testCase "simple textcite" $
    runParser (inlines <* eof)
      [Command "textcite" [OblArg [Plain "one"]]]
    @?=
    Right [Citation (MultiCite CiteText [] []
      [SingleCite [] [] ["one"]]) Nothing]
  , testCase "simple parencite" $
    runParser (inlines <* eof)
      [Command "parencite" [OblArg [Plain "one"]]]
    @?=
    Right [Citation (MultiCite CiteParen [] []
      [SingleCite [] [] ["one"]]) Nothing]
  , testCase "nocite disappears" $
    runParser (inlines <* eof)
      [Command "nocite" [OblArg [Plain "one"]]]
    @?=
    Right []
  , testCase "citation keys are added to meta" $
    either (error . show) (M.lookup "zero" . metaCiteMap . snd)
      (runParserWithState (inlines <* eof)
        [Command "cite" [OblArg [Plain "zero"]]])
    @?=
    Just 0
  , testCase "nocite keys are added to meta" $
    either (error . show) (M.lookup "one" . metaCiteMap . snd)
      (runParserWithState (inlines <* eof)
        [Command "nocite" [OblArg [Plain "zero,", White, Plain "one"]]])
    @?=
    Just 1
  ]

testsMath :: Test
testsMath = testGroup "math"
  [ testCase "simple math" $
    runParser (inlines <* eof)
      [MathGroup MathInline [Plain "a+b"]]
    @?=
    Right [Math MathInline [Str "a+b"]]
  , testCase "math with superscript" $
    runParser (inlines <* eof)
      [MathGroup MathDisplay [Plain "c",SupScript [Plain "2"]]]
    @?=
    Right [Math MathDisplay [Str "c", FontStyle Sup [Str "2"]]]
  , testCase "math with subscript" $
    runParser (inlines <* eof)
      [MathGroup MathDisplay [Plain "M",SubScript [Plain "i,j"]]]
    @?=
    Right [Math MathDisplay [Str "M", FontStyle Sub [Str "i,j"]]]
  , testCase "math with special symbols" $
    runParser (inlines <* eof)
      [MathGroup MathInline [Plain "p\x2228\x00AC\&q"]]
    @?=
    Right [Math MathInline [Str "p\x2228\x00AC\&q"]]
  ]

testsSections :: Test
testsSections = testGroup "sections"
  [ testCase "one chapter in every book part" $
    runParser (skipInterlevel *> blocks <* eof)
      [ Command "frontmatter" []
      , Command "chapter" [OblArg [Plain "front-one"]]
      , Command "mainmatter" []
      , Command "chapter" [OblArg [Plain "main-one"]]
      , Command "appendix" []
      , Command "chapter" [OblArg [Plain "back-one"]]
      , Command "backmatter" []
      , Command "chapter" [OblArg [Plain "back-two"]]
      ]
    @?=
    Right
      [ Header 2 (SectionAnchor (SectionInfo Frontmatter
          (SectionRegular (0,1,0,0,0,0,0)))) [Str "front-one"]
      , Header 2 (SectionAnchor (SectionInfo Mainmatter
          (SectionRegular (0,1,0,0,0,0,0)))) [Str "main-one"]
      , Header 2 (SectionAnchor (SectionInfo Backmatter
          (SectionRegular (0,1,0,0,0,0,0)))) [Str "back-one"]
      , Header 2 (SectionAnchor (SectionInfo Backmatter
          (SectionRegular (0,2,0,0,0,0,0)))) [Str "back-two"]
      ]
  , testCase "phantom chapter between regular chapters" $
    runParser (blocks <* eof)
      [ Command "chapter" [OblArg [Plain "one"]]
      , Command "chapter" [StarArg, OblArg [Plain "interlude"]]
      , Command "chapter" [OblArg [Plain "two"]]
      ]
    @?=
    Right
      [ Header 2 (SectionAnchor (SectionInfo Frontmatter
          (SectionRegular (0,1,0,0,0,0,0)))) [Str "one"]
      , Header 2 (SectionAnchor (SectionInfo Frontmatter
          (SectionPhantom 1))) [Str "interlude"]
      , Header 2 (SectionAnchor (SectionInfo Frontmatter
          (SectionRegular (0,2,0,0,0,0,0)))) [Str "two"]
      ]
  , testCase "phantom chapters use a global counter" $
    runParser (block *> block <* eof)
      [ Command "chapter" [StarArg, OblArg [Plain "one"]]
      , Command "mainmatter" []
      , Command "chapter" [StarArg, OblArg [Plain "two"]]
      ]
    @?=
    Right
      ( Header 2 (SectionAnchor (SectionInfo Mainmatter
          (SectionPhantom 2))) [Str "two"])
  , testCase "starred 'addchap' (KOMA-script) introduces phantom chapter" $
    runParser (block *> block <* eof)
      [ Command "chapter" [StarArg, OblArg [Plain "one"]]
      , Command "mainmatter" []
      , Command "addchap" [StarArg, OblArg [Plain "two"]]
      ]
    @?=
    Right
      ( Header 2 (SectionAnchor (SectionInfo Mainmatter
          (SectionPhantom 2))) [Str "two"])
  , testCase "unstarred 'addchap' (KOMA-script) introduces phantom chapter" $
    runParser (block *> block <* eof)
      [ Command "chapter" [StarArg, OblArg [Plain "one"]]
      , Command "mainmatter" []
      , Command "addchap" [OblArg [Plain "two"]]
      ]
    @?=
    Right
      ( Header 2 (SectionAnchor (SectionInfo Mainmatter
          (SectionPhantom 2))) [Str "two"])
  , testCase "unstarred 'addpart' (KOMA-script) introduces phantom part" $
    runParser (block *> block <* eof)
      [ Command "chapter" [StarArg, OblArg [Plain "one"]]
      , Command "mainmatter" []
      , Command "addpart" [OblArg [Plain "two"]]
      ]
    @?=
    Right
      ( Header 1 (SectionAnchor (SectionInfo Mainmatter
          (SectionPhantom 2))) [Str "two"])
  ]

testsLists :: Test
testsLists = testGroup "list blocks"
  [ testCase "simple unordered list" $
    runParser (inlines *> itemize) exampleList1
    @?=
    Right (List UnorderedList
      [ [Para [Str "one",Space,Str "one"]]
      , [Para [Str "two",Space]]
      , [Para [Str "three"]]])
  , testCase "simple ordered list" $
    runParser (blocks <* eof)
      [Group "enumerate" []
        [Command "item" [], Plain "one", White, Plain "one"
        ,Command "item" [], Plain "two"]]
    @?=
    Right [List OrderedList
      [ [Para [Str "one",Space,Str "one"]]
      , [Para [Str "two"]]]]
  , testCase "itemize list with leading and trailing par breaks" $
    runParser (blocks <* eof)
      [Group "itemize" []
        [Par
        ,Command "item" [], Plain "one", White, Plain "one"
        ,Command "item" [], Plain "two", White], Par]
    @?=
    Right [List UnorderedList
      [ [Para [Str "one",Space,Str "one"]]
      , [Para [Str "two",Space]]]]
  , testCase "itemize list with many par breaks" $
    runParser (blocks <* eof)
      [Group "itemize" []
        [Par
        ,Command "item" [], Par, Plain "one", Par, Plain "one"
        ,Command "item" [], Par, Plain "two", Par], Par]
    @?=
    Right [List UnorderedList
      [ [Para [Str "one"], Para [Str "one"]]
      , [Para [Str "two"]]]]
  , testCase "enumerate list with leading par break" $
    runParser (blocks <* eof)
      [Group "enumerate" []
        [Par, Command "item" [], Plain "one", White]]
    @?=
    Right [List OrderedList
      [[Para [Str "one",Space]]]]
  , testCase "enumerate list with par break after 'item' command" $
    runParser (blocks <* eof)
      [Group "enumerate" []
        [Command "item" [], Par, Plain "one"]]
    @?=
    Right [List OrderedList
      [[Para [Str "one"]]]]
  , testCase "nested list" $
    runParser (inlines *> itemize) exampleList2
    @?=
    Right (List UnorderedList
      [ [ Para [ Str "up-one"]]
      , [ Para [ Str "up-two",Space]
        , List UnorderedList
          [ [Para [Str "down-one",Space]]
          , [Para [Str "down-two",Space]]]]
      , [ Para [ Str "up-three"]]])
  ]

testsListItems :: Test
testsListItems = testGroup "list item blocks"
  [ testCase "empty item list" $
    runParser block [Group "exe" [] []]
    @?=
    Right (ListItemBlock [])
  , testCase "item list with single item" $
    runParser block [Group "exe" []
      [ Command "ex" [], Plain "hello", White, Plain "world" ]]
    @?=
    Right (ListItemBlock
      [ListItem (ItemAnchor (0,[1]))
        [Para [Str "hello", Space, Str "world"]]])
  , testCase "item list with single multi-paragraph item" $
    runParser block [Group "exe" []
      [ Command "ex" [], Par, Plain "hello", Par, White, Plain "world" ]]
    @?=
    Right (ListItemBlock
      [ListItem (ItemAnchor (0,[1]))
        [Para [Str "hello"], Para [Str "world"]]])
  , testCase "item list with two items" $
    runParser block [Group "exe" []
      [ Command "ex" [], Plain "one"
      , Command "ex" [], Plain "two"]]
    @?=
    Right (ListItemBlock
      [ ListItem (ItemAnchor (0,[1])) [Para [Str "one"]]
      , ListItem (ItemAnchor (0,[2])) [Para [Str "two"]]])
  , testCase "item list with two items in sublist" $
    runParser block [Group "exe" []
      [ Command "ex" [], Group "xlist" []
        [ Command "ex" [], Plain "sub-one"
        , Command "ex" [], Plain "sub-two"]]]
    @?=
    Right (ListItemBlock
      [ ListItem (ItemAnchor (0,[1]))
        [ ListItemBlock
          [ ListItem (ItemAnchor (0,[1,1])) [Para [Str "sub-one"]]
          , ListItem (ItemAnchor (0,[2,1])) [Para [Str "sub-two"]]]]])
  , testCase "item list with multiple nested sublists" $
    runParser blocks [Group "exe" []
      [ Command "ex" [], Plain "one", Group "xlist" []
        [ Command "ex" [], Plain "one-one"
        , Command "ex" [], Plain "one-two"]
      , Command "ex" [], Plain "two", Group "xlist" []
        [ Command "ex" [], Plain "two-one", Group "xlist" []
          [ Command "ex" [], Plain "two-one-one"
          , Command "ex" [], Plain "two-one-two"]
        , Command "ex" [], Plain "two-two"]]]
    @?=
    Right [ListItemBlock
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
  , testCase "anchor map is updated with top-level item label" $
    either (error . show) (M.lookup "my-label" . metaAnchorMap . snd)
      (runParserWithState (blocks <* eof) [Group "exe" []
        [ Command "ex" [], Plain "hello"
        , Command "label" [OblArg [Plain "my-label"]]]])
    @?=
    Just (ItemAnchor (0,[1]))
  , testCase "anchor map is updated with embedded item label" $
    either (error . show) (M.lookup "my-label" . metaAnchorMap . snd)
      (runParserWithState (blocks <* eof) [Group "exe" []
        [ Command "ex" [], Plain "outer", Group "xlist" []
          [ Command "ex" [], Plain "inner-1"
          , Command "ex" [], Plain "inner-2"
          , Command "label" [OblArg [Plain "my-label"]]]]])
    @?=
    Just (ItemAnchor (0,[2,1]))
  , testCase "chapter number is stored in item counter" $
    either (error . show) (M.lookup "my-label" . metaAnchorMap . snd)
      (runParserWithState (blocks <* eof)
        [ Command "chapter" [OblArg [Plain "one"]]
        , Command "chapter" [OblArg [Plain "two"]]
        , Group "exe" []
          [ Command "ex" []
          , Command "label" [OblArg [Plain "my-label"]]]])
    @?=
    Just (ItemAnchor (2,[1]))
  , testCase "item counter is incremented within chapter" $
    either (error . show) (M.lookup "my-label" . metaAnchorMap . snd)
      (runParserWithState (blocks <* eof)
        [ Command "chapter" [OblArg [Plain "one"]]
        , Group "exe" [] [ Command "ex" [], Plain "example"]
        , Group "exe" [] [ Command "ex" [], Plain "example"
          , Command "label" [OblArg [Plain "my-label"]]]])
    @?=
    Just (ItemAnchor (1,[2]))
  , testCase "item counter is reset at chapter boundaries" $
    either (error . show) (M.lookup "my-label" . metaAnchorMap . snd)
      (runParserWithState (blocks <* eof)
        [ Command "chapter" [OblArg [Plain "one"]]
        , Group "exe" [] [ Command "ex" [], Plain "example"]
        , Group "exe" [] [ Command "ex" [], Plain "example"]
        , Command "chapter" [OblArg [Plain "two"]]
        , Group "exe" [] [ Command "ex" [], Plain "example"
          , Command "label" [OblArg [Plain "my-label"]]]])
    @?=
    Just (ItemAnchor (2,[1]))
  ]

testsInterlinear :: Test
testsInterlinear = testGroup "interlinear glossed text"
  [ testCase "two lines with one word" $
    runParser block
      [ Command "gll" []
      , Plain "one", Newline
      , Plain "two", Newline]
    @?=
    Right (SimpleTable [[SingleCell [Str "one"]], [SingleCell [Str "two"]]])
  , testCase "two lines with two words" $
    runParser block
      [ Command "gll" []
      , Plain "one", White, Plain "one", Newline
      , Plain "two", White, Plain "two", White, Newline]
    @?=
    Right (SimpleTable [ [SingleCell [Str "one"], SingleCell [Str "one"]]
                       , [SingleCell [Str "two"], SingleCell [Str "two"]]])
  , testCase "two lines with translation line" $
    runParser block
      [ Command "gll" []
      , Plain "one", White, Plain "one", Newline
      , Plain "two", White, Plain "two", Newline
      , Command "trans" [], Plain "translation", White, Plain "line", Par]
    @?=
    Right (SimpleTable [ [SingleCell [Str "one"], SingleCell [Str "one"]]
                       , [SingleCell [Str "two"], SingleCell [Str "two"]]
                       , [MultiCell 2 [Str "translation", Space, Str "line"]]])
  , testCase "misaligned lines with translation" $
    runParser block
      [ Command "gll" []
      , Plain "one", Newline
      , Plain "two", White, Plain "two", White, Plain "two", Newline
      , Command "trans" [], Plain "translation", White, Plain "line", Par]
    @?=
    Right (SimpleTable [ [SingleCell [Str "one"]]
                       , replicate 3 (SingleCell [Str "two"])
                       , [MultiCell 3 [Str "translation", Space, Str "line"]]])
  , testCase "four lines without translation" $
    runParser block
      [ Command "gllll" []
      , Plain "one", White, Plain "one", Newline
      , Plain "two", White, Plain "two", White, Newline
      , Plain "three", White, Plain "three", White, Newline
      , Plain "four", White, Plain "four", Newline]
    @?=
    Right (SimpleTable [ replicate 2 (SingleCell [Str "one"])
                       , replicate 2 (SingleCell [Str "two"])
                       , replicate 2 (SingleCell [Str "three"])
                       , replicate 2 (SingleCell [Str "four"])])
  , testCase "two lines with composed words" $
    runParser block
      [ Command "gll" []
      , Group "" [] [Plain "one", White, Plain "one"], White, Plain "one", Newline
      , Plain "two", White, Plain "two", Newline]
    @?=
    Right (SimpleTable [ [SingleCell [Str "one", Space, Str "one"], SingleCell [Str "one"]]
                       , replicate 2 (SingleCell [Str "two"])])
  , testCase "adjacent groups form a single word" $
    runParser block
      [ Command "gll" []
      , Group "" [] [Plain "one", White, Plain "one"], Plain "one", White
      , Plain "one", Newline
      , Plain "two", White
      , Group "" [] [Plain "two"], Group "" [] [Plain "two", White, Plain "two"], Newline]
    @?=
    Right (SimpleTable
      [ [SingleCell [Str "one", Space, Str "one", Str "one"], SingleCell [Str "one"]]
      , [SingleCell [Str "two"], SingleCell [Str "two", Str "two", Space, Str "two"]]])
  , testCase "empty group is treated as a word" $
    runParser block
      [ Command "gll" []
      , Group "" [] [], White, Plain "one", Newline
      , Plain "two", Newline]
    @?=
    Right (SimpleTable
      [ [SingleCell [], SingleCell [Str "one"]]
      , [SingleCell [Str "two"]]])
  , testCase "whitespace within group is treated as a word" $
    runParser block
      [ Command "gll" []
      , Group "" [] [White], White, Plain "one", Newline
      , Plain "two", Newline]
    @?=
    Right (SimpleTable
      [ [SingleCell [Space], SingleCell [Str "one"]]
      , [SingleCell [Str "two"]]])
  ]

testsFigures :: Test
testsFigures = testGroup "figures"
  [ testCase "minimal figure: no whitespace, no label" $
    runParser (blocks <* eof) [Group "figure" []
      [ Command "includegraphics" [OblArg [Plain "file.png"]]
      , Command "caption" [OblArg [Plain "description"]]]]
    @?=
    Right [Figure (FigureAnchor (0,1)) "file.png" [Str "description"]]
  , testCase "figure with centering and whitespace" $
    runParser (blocks <* eof) [Group "figure" []
      [ White, Command "centering" []
      , Command "includegraphics" [OblArg [Plain "file.png"]]
      , Par, Command "caption" [OblArg [Plain "description"]]]]
    @?=
    Right [Figure (FigureAnchor (0,1)) "file.png" [Str "description"]]
  , testCase "figure with center group" $
    runParser (blocks <* eof) [Group "figure" [] [Group "center" []
      [ Command "includegraphics" [OblArg [Plain "file.png"]]
      , Par, Command "caption" [OblArg [Plain "description"]]]]]
    @?=
    Right [Figure (FigureAnchor (0,1)) "file.png" [Str "description"]]
  , testCase "figure with smaller center group" $
    runParser (blocks <* eof) [Group "figure" []
      [ Group "center" []
        [ Command "includegraphics" [OblArg [Plain "file.png"]]]
      , Command "caption" [OblArg [Plain "description"]]]]
    @?=
    Right [Figure (FigureAnchor (0,1)) "file.png" [Str "description"]]
  , testCase "figure with label after caption" $
    runParser (blocks <* eof) [Group "figure" []
      [ Command "includegraphics" [OblArg [Plain "file.png"]]
      , Command "caption" [OblArg [Plain "description"]]
      , Command "label" [OblArg [Plain "figlabel"]]]]
    @?=
    Right [Figure (FigureAnchor (0,1)) "file.png" [Str "description"]]
  , testCase "figure with label before caption" $
    runParser (blocks <* eof) [Group "figure" []
      [ Command "includegraphics" [OblArg [Plain "file.png"]]
      , Command "label" [OblArg [Plain "figlabel"]]
      , Command "caption" [OblArg [Plain "description"]]]]
    @?=
    Right [Figure (FigureAnchor (0,1)) "file.png" [Str "description"]]
  , testCase "anchor map is updated with figure label" $
    either (error . show) (M.lookup "figlabel" . metaAnchorMap . snd)
      (runParserWithState (blocks <* eof) [Group "figure" []
        [ Command "includegraphics" [OblArg [Plain "file.png"]]
        , Command "caption" [OblArg [Plain "description"]]
        , Command "label" [OblArg [Plain "figlabel"]]]])
    @?=
    Just (FigureAnchor (0,1))
  , testCase "chapter number is stored in figure counter" $
    either (error . show) (M.lookup "figlabel" . metaAnchorMap . snd)
      (runParserWithState (blocks <* eof)
        [ Command "chapter" [OblArg [Plain "one"]]
        , Command "chapter" [OblArg [Plain "two"]]
        , Group "figure" []
          [ Command "includegraphics" [OblArg [Plain "file.png"]]
          , Command "caption" [OblArg [Plain "description"]]
          , Command "label" [OblArg [Plain "figlabel"]]]])
    @?=
    Just (FigureAnchor (2,1))
  , testCase "figure counter is incremented within chapter" $
    either (error . show) (M.lookup "figlabel" . metaAnchorMap . snd)
      (runParserWithState (blocks <* eof)
        [ Command "chapter" [OblArg [Plain "one"]]
        , Group "figure" []
          [ Command "includegraphics" [OblArg [Plain "file.png"]]
          , Command "caption" [OblArg [Plain "description"]]]
        , Group "figure" []
          [ Command "includegraphics" [OblArg [Plain "file.png"]]
          , Command "caption" [OblArg [Plain "description"]]
          , Command "label" [OblArg [Plain "figlabel"]]]])
    @?=
    Just (FigureAnchor (1,2))
  , testCase "figure counter is reset at chapter boundaries" $
    either (error . show) (M.lookup "figlabel" . metaAnchorMap . snd)
      (runParserWithState (blocks <* eof)
        [ Command "chapter" [OblArg [Plain "one"]]
        , Group "figure" []
          [ Command "includegraphics" [OblArg [Plain "file.png"]]
          , Command "caption" [OblArg [Plain "description"]]]
        , Group "figure" []
          [ Command "includegraphics" [OblArg [Plain "file.png"]]
          , Command "caption" [OblArg [Plain "description"]]]
        , Command "chapter" [OblArg [Plain "two"]]
        , Group "figure" []
          [ Command "includegraphics" [OblArg [Plain "file.png"]]
          , Command "caption" [OblArg [Plain "description"]]
          , Command "label" [OblArg [Plain "figlabel"]]]])
    @?=
    Just (FigureAnchor (2,1))
  ]

testsTables :: Test
testsTables = testGroup "tables"
  [ testCase "empty table" $
    runParser (blocks <* eof) [Group "table" []
      [ Group "tabular" [] []
      , Command "caption" [OblArg [Plain "description"]]]]
    @?=
    Right [Table (TableAnchor (0,1)) [Str "description"] []]
  , testCase "single cell table" $
    runParser (blocks <* eof) [Group "table" []
      [ Group "tabular" [] [Plain "hello", Newline]
      , Command "caption" [OblArg [Plain "description"]]]]
    @?=
    Right [Table (TableAnchor (0,1)) [Str "description"]
      [[SingleCell [Str "hello"]]]]
  , testCase "simple 2x2 tabular: no whitespace, no label" $
    runParser tabular
      [ Group "tabular" []
        [ Plain "top-left", AlignMark, Plain "top-right", Newline
        , Plain "bottom-left", AlignMark, Plain "bottom-right", Newline]]
    @?=
    Right
      [[SingleCell [Str "top-left"], SingleCell [Str "top-right"]]
      ,[SingleCell [Str "bottom-left"], SingleCell [Str "bottom-right"]]]
  , testCase "simple 2x2 table: no whitespace, no label" $
    runParser table [Group "table" []
      [ Group "tabular" []
        [ Plain "top-left", AlignMark, Plain "top-right", Newline
        , Plain "bottom-left", AlignMark, Plain "bottom-right", Newline]
      , Command "caption" [OblArg [Plain "description"]]]]
    @?=
    Right (Table (TableAnchor (0,1)) [Str "description"]
      [[SingleCell [Str "top-left"], SingleCell [Str "top-right"]]
      ,[SingleCell [Str "bottom-left"], SingleCell [Str "bottom-right"]]])
  , testCase "table with some empty cells" $
    runParser (blocks <* eof) [Group "table" []
      [ Group "tabular" []
        [ Plain "top-left", AlignMark, Newline
        , AlignMark, Plain "bottom-right", Newline]
      , Command "caption" [OblArg [Plain "description"]]]]
    @?=
    Right [Table (TableAnchor (0,1)) [Str "description"]
      [[SingleCell [Str "top-left"], SingleCell []]
      ,[SingleCell [], SingleCell [Str "bottom-right"]]]]
  , testCase "skeleton table with inline whitespace" $
    runParser (blocks <* eof) [Group "table" []
      [ White, Group "tabular" []
        [ White, AlignMark, Newline
        , White, AlignMark, White, Newline]
      , Command "caption" [OblArg [Plain "description"]]]]
    @?=
    Right [Table (TableAnchor (0,1)) [Str "description"]
      [[SingleCell [], SingleCell []]
      ,[SingleCell [], SingleCell []]]]
  , testCase "simple ragged table" $
    runParser (blocks <* eof) [Group "table" []
      [ Group "tabular" []
        [ Plain "top-left", Newline
        , Plain "bottom-left", AlignMark, Plain "bottom-right", Newline]
      , Command "caption" [OblArg [Plain "description"]]]]
    @?=
    Right [Table (TableAnchor (0,1)) [Str "description"]
      [[SingleCell [Str "top-left"]]
      ,[SingleCell [Str "bottom-left"], SingleCell [Str "bottom-right"]]]]
  , testCase "empty ragged table" $
    runParser (blocks <* eof) [Group "table" []
      [ White, Group "tabular" []
        [ White, AlignMark, Newline
        , White, AlignMark, AlignMark, AlignMark, White, Newline]
      , Command "caption" [OblArg [Plain "description"]]]]
    @?=
    Right [Table (TableAnchor (0,1)) [Str "description"]
      [[SingleCell [], SingleCell []]
      ,[SingleCell [], SingleCell [], SingleCell [], SingleCell []]]]
  , testCase "table with centering" $
    runParser table [Group "table" []
      [ White, Command "centering" []
      , Group "tabular" []
        [ Plain "top-left", AlignMark, Plain "top-right", Newline
        , Plain "bottom-left", AlignMark, Plain "bottom-right", Newline]
      , Par
      , Command "caption" [OblArg [Plain "description"]]
      , Par]]
    @?=
    Right (Table (TableAnchor (0,1)) [Str "description"]
      [[SingleCell [Str "top-left"], SingleCell [Str "top-right"]]
      ,[SingleCell [Str "bottom-left"], SingleCell [Str "bottom-right"]]])
  , testCase "table with center group" $
    runParser table [Group "table" []
      [ Group "center" []
        [ Group "tabular" []
          [ Plain "top-left", AlignMark, Plain "top-right", Newline
          , Plain "bottom-left", AlignMark, Plain "bottom-right", Newline]
        , Par
        , Command "caption" [OblArg [Plain "description"]]]]]
    @?=
    Right (Table (TableAnchor (0,1)) [Str "description"]
      [[SingleCell [Str "top-left"], SingleCell [Str "top-right"]]
      ,[SingleCell [Str "bottom-left"], SingleCell [Str "bottom-right"]]])
  , testCase "table with multi-inline cells" $
    runParser table [Group "table" []
      [ Group "tabular" []
        [ Plain "top", White, Plain "left", AlignMark
        , Plain "top", White, Plain "right", Newline
        , Plain "bottom", White, Plain "left", AlignMark
        , Plain "bottom", White, Plain "right", Newline]
      , Command "caption" [OblArg [Plain "longer", White, Plain "description"]]]]
    @?=
    Right (Table (TableAnchor (0,1)) [Str "longer", Space, Str "description"]
      [ [ SingleCell [Str "top", Space, Str "left"]
        , SingleCell [Str "top", Space, Str "right"]]
      , [ SingleCell [Str "bottom", Space, Str "left"]
        , SingleCell [Str "bottom", Space, Str "right"]]])
  , testCase "table with hline commands" $
    runParser table [Group "table" []
      [ White, Command "centering" []
      , Group "tabular" []
        [ Command "hline" []
        , Plain "top-left", AlignMark, Plain "top-right", Newline
        , Command "hline" []
        , Plain "bottom-left", AlignMark, Plain "bottom-right", Newline
        , Command "hline" []]
      , Command "caption" [OblArg [Plain "description"]]]]
    @?=
    Right (Table (TableAnchor (0,1)) [Str "description"]
      [[SingleCell [Str "top-left"], SingleCell [Str "top-right"]]
      ,[SingleCell [Str "bottom-left"], SingleCell [Str "bottom-right"]]])
  , testCase "paragraph breaks between tabular rows" $
    -- in fact, LaTeX allows paragraph breaks within table cells
    -- and treats them as ordinary inline whitespace
    runParser table [Group "table" []
      [ White, Command "centering" []
      , Group "tabular" []
        [ Plain "1-1", AlignMark, Plain "1-2", Newline
        , Command "hline" []
        , Par
        , Plain "2-1", AlignMark, Plain "2-2", Newline
        , Par
        , Plain "3-1", AlignMark, Plain "3-2", Newline
        , Par]
      , Command "caption" [OblArg [Plain "description"]]]]
    @?=
    Right (Table (TableAnchor (0,1)) [Str "description"]
      [[SingleCell [Str "1-1"], SingleCell [Str "1-2"]]
      ,[SingleCell [Str "2-1"], SingleCell [Str "2-2"]]
      ,[SingleCell [Str "3-1"], SingleCell [Str "3-2"]]])
  , testCase "tabular with single multicolumn cell" $
    runParser tabular
      [ Group "tabular" []
        [ Command "multicolumn"
          [ OblArg [Plain "3"]
          , OblArg [Plain "c"]
          , OblArg [Plain "three", White, Plain "cells"]]
        , Newline]]
    @?=
    Right [[MultiCell 3 [Str "three", Space, Str "cells"]]]
  , testCase "multicolumn cells" $
    runParser (blocks <* eof) [Group "table" []
      [ Group "tabular" []
        [ Plain "1-1", AlignMark, Plain "1-2", AlignMark, Plain "1-3", Newline
        , Command "multicolumn"
          [ OblArg [Plain "2"], OblArg [Plain "c"], OblArg [Plain "2-(1,2)"]]
        , AlignMark, Plain "2-3", Newline
        , Command "multicolumn"
          [ OblArg [Plain "3"], OblArg [Plain "l"], OblArg [Plain "3-(1,2,3)"]]
        , Newline, Plain "4-1", AlignMark
        , Command "multicolumn"
          [ OblArg [Plain "2"], OblArg [Plain "c"], OblArg [Plain "4-(2,3)"]]
        , Newline]
      , Command "caption" [OblArg [Plain "description"]]]]
    @?=
    Right [Table (TableAnchor (0,1)) [Str "description"]
      [[SingleCell [Str "1-1"], SingleCell [Str "1-2"], SingleCell [Str "1-3"]]
      ,[MultiCell 2 [Str "2-(1,2)"], SingleCell [Str "2-3"]]
      ,[MultiCell 3 [Str "3-(1,2,3)"]]
      ,[SingleCell [Str "4-1"], MultiCell 2 [Str "4-(2,3)"]]]]
  , testCase "empty table with label before caption" $
    runParser (blocks <* eof) [Group "table" []
      [ Group "tabular" [] []
      , Command "label" [OblArg [Plain "table-label"]]
      , Command "caption" [OblArg [Plain "description"]]]]
    @?=
    Right [Table (TableAnchor (0,1)) [Str "description"] []]
  , testCase "anchor map is updated with table label" $
    either (error . show) (M.lookup "table-label" . metaAnchorMap . snd)
      (runParserWithState (blocks <* eof) [Group "table" []
      [ Group "tabular" [] []
      , Command "caption" [OblArg [Plain "description"]]
      , Command "label" [OblArg [Plain "table-label"]]]])
    @?=
    Just (TableAnchor (0,1))
  , testCase "chapter number is stored in table counter" $
    either (error . show) (M.lookup "table-label" . metaAnchorMap . snd)
      (runParserWithState (blocks <* eof)
        [ Command "chapter" [OblArg [Plain "one"]]
        , Command "chapter" [OblArg [Plain "two"]]
        , Group "table" []
          [ Group "tabular" [] []
          , Command "caption" [OblArg [Plain "description"]]
          , Command "label" [OblArg [Plain "table-label"]]]])
    @?=
    Just (TableAnchor (2,1))
  , testCase "table counter is incremented within chapter" $
    either (error . show) (M.lookup "table-label" . metaAnchorMap . snd)
      (runParserWithState (blocks <* eof)
        [ Command "chapter" [OblArg [Plain "one"]]
        , Group "table" []
          [ Group "tabular" [] []
          , Command "caption" [OblArg [Plain "description"]]]
        , Group "table" []
          [ Group "tabular" [] []
          , Command "caption" [OblArg [Plain "description"]]
          , Command "label" [OblArg [Plain "table-label"]]]])
    @?=
    Just (TableAnchor (1,2))
  , testCase "table counter is reset at chapter boundaries" $
    either (error . show) (M.lookup "table-label" . metaAnchorMap . snd)
      (runParserWithState (blocks <* eof)
        [ Command "chapter" [OblArg [Plain "one"]]
        , Group "table" []
          [ Group "tabular" [] []
          , Command "caption" [OblArg [Plain "description"]]]
        , Group "table" []
          [ Group "tabular" [] []
          , Command "caption" [OblArg [Plain "description"]]]
        , Command "chapter" [OblArg [Plain "two"]]
        , Group "table" []
          [ Group "tabular" [] []
          , Command "caption" [OblArg [Plain "description"]]
          , Command "label" [OblArg [Plain "table-label"]]]])
    @?=
    Just (TableAnchor (2,1))
  ]

testsFootnotes :: Test
testsFootnotes = testGroup "footnotes"
  [ testCase "empty footnote" $
    runParser (inlines <* eof) [Command "footnote" []]
    @?=
    Right [Note (NoteAnchor (0,1)) []]
  , testCase "simple footnote" $
    runParser (inlines <* eof) [Command "footnote" [OblArg [Plain "hello"]]]
    @?=
    Right [Note (NoteAnchor (0,1)) [Para [Str "hello"]]]
  , testCase "multi-paragraph footnote" $
    runParser (inlines <* eof)
      [Command "footnote" [OblArg [Plain "one", Par, Plain "two"]]]
    @?=
    Right [Note (NoteAnchor (0,1)) [Para [Str "one"], Para [Str "two"]]]
  , testCase "note counter is incremented within chapter" $
    runParser (block *> inline *> inline <* eof)
      [ Command "chapter" [OblArg [Plain "one"]]
      , Command "footnote" [OblArg [Plain "fn-one"]]
      , Command "footnote" [OblArg [Plain "fn-two"]]]
    @?=
    Right (Note (NoteAnchor (1,2)) [Para [Str "fn-two"]])
  , testCase "note counter is reset at chapter boundaries" $
    runParser (block *> inline *> inline *> block *> inline <* eof)
      [ Command "chapter" [OblArg [Plain "one"]]
      , Command "footnote" [OblArg [Plain "fn-1-1"]]
      , Command "footnote" [OblArg [Plain "fn-1-2"]]
      , Command "chapter" [OblArg [Plain "two"]]
      , Command "footnote" [OblArg [Plain "fn-2-1"]]]
    @?=
    Right (Note (NoteAnchor (2,1)) [Para [Str "fn-2-1"]])
  ]

testsCrossrefs :: Test
testsCrossrefs = testGroup "cross-references"
  [ testCase "labels are dropped" $
    runParser (inlines <* eof)
      [Command "label" [OblArg [Plain "mylabel"]]]
    @?=
    Right []
  , testCase "labels between spaces do not prevent whitespace conflation" $
    runParser (inlines <* eof)
      [White, Command "label" [OblArg [Plain "mylabel"]], White]
    @?=
    Right [Space]
  , testCase "spaces after labels are not dropped" $
    runParser (inlines <* eof)
      [Plain "a", Command "label" [OblArg [Plain "mylabel"]], White]
    @?=
    Right [Str "a", Space]
  , testCase "spaces after labels are conflated" $
    runParser (inlines <* eof)
      [Plain "a", Command "label" [OblArg [Plain "mylabel"]], White, White]
    @?=
    Right [Str "a", Space]
  , testCase "labels between plain strings are dropped by inlines parser" $
    runParser (inlines <* eof)
      [ Plain "a"
      , Command "label" [OblArg [Plain "mylabel"]]
      , Plain "b"]
    @?=
    Right [Str "a", Str "b"]
  , testCase "labels between plain strings are dropped by blocks parser" $
    runParser (blocks <* eof)
      [ Plain "a"
      , Command "label" [OblArg [Plain "mylabel"]]
      , Plain "b"]
    @?=
    Right [Para [Str "a", Str "b"]]
  , testCase "labels may contain whitespace" $
    runParser (inlines <* eof)
      [Command "label" [OblArg [Plain "my", White, Plain "label"]]]
    @?=
    Right []
  , testCase "labels may contain underscores" $
    -- This reflects a workaround for allowing underscores in label names.
    -- Once there is a mechanism for handling verbatim text in the parser,
    -- it should be used for label names as well.
    runParser (inlines <* eof)
      [Command "label" [OblArg
        [Plain "my", SubScript [Plain "l"], Plain "abel"]]]
    @?=
    Right []
  , testCase "simple pointer with undefined target" $
    runParser block
      [Command "ref" [OblArg [Plain "nosuchtarget"]]]
    @?=
    Right (Para [Pointer "nosuchtarget" Nothing])
  , testCase "pointer name with whitespace and underscore" $
    runParser block
      [Command "ref" [OblArg
        [Plain "a", SubScript [Plain "b"], Plain "2", White, Plain "c" ]]]
    @?=
    Right (Para [Pointer "a_b2 c" Nothing])
  , testCase "section label and reference" $
    runParser (blocks <* eof)
      [ Command "section" [OblArg [Plain "one"]]
      , Command "label" [OblArg [Plain "mylabel"]]
      , Command "ref" [OblArg [Plain "mylabel"]]
      ]
    @?=
    Right
      [ Header 3
          (SectionAnchor (SectionInfo Frontmatter
            (SectionRegular (0,0,1,0,0,0,0))))
          [Str "one"]
      , Para [Pointer "mylabel" Nothing]]
  , testCase "retrieve section number" $
    either (error . show) (M.lookup "mylabel" . metaAnchorMap . snd)
      (runParserWithState (blocks <* eof)
        [ Command "section" [OblArg [Plain "one"]]
        , Command "section" [OblArg [Plain "two"]]
        , Command "subsection" [OblArg [Plain "two-one"]]
        , Command "label" [OblArg [Plain "mylabel"]]
        , Command "section" [OblArg [Plain "three"]]
        , Command "section" [OblArg [Plain "four"]]
        , Command "ref" [OblArg [Plain "mylabel"]]
        ])
    @?=
    Just (SectionAnchor (SectionInfo Frontmatter
      (SectionRegular (0,0,2,1,0,0,0))))
  , testCase "register label with underscore and whitespace" $
    either (error . show) (M.lookup "my_label 1" . metaAnchorMap . snd)
      (runParserWithState (blocks <* eof)
        [ Command "section" [OblArg [Plain "one"]]
        , Command "label" [OblArg
            [ Plain "my", SubScript [Plain "l"], Plain "abel"
            , White, Plain "1"]]
        ])
    @?=
    Just (SectionAnchor (SectionInfo Frontmatter
      (SectionRegular (0,0,1,0,0,0,0))))
  ]

testsHyperref :: Test
testsHyperref = testGroup "hyperref package"
  [ testCase "simple href command" $
    runParser (inlines <* eof)
      [Command "href" [OblArg [Plain "http://example.com/"]
                      ,OblArg [Plain "some", White, Plain "description"]]]
    @?=
    Right [Pointer "external" (Just (ExternalResource
      [Str "some", Space, Str "description"] "http://example.com/" "" ""))]
  , testCase "simple url command" $
    runParser (inlines <* eof)
      [Command "url" [OblArg [Plain "http://example.com/"]]]
    @?=
    Right [Pointer "external" (Just (ExternalResource
      [Str "http://example.com/"] "http://example.com/" "" ""))]
  ]

testsWhitespace :: Test
testsWhitespace = testGroup "whitespace"
  [ testCase "inline whitespace is conflated" $
    runParser (inlines <* eof)
      [White, White]
    @?=
    Right [Space]
  , testCase "inline whitespace within paragraphs is conflated" $
    runParser (blocks <* eof)
      [Plain "a", White, White, Plain "b"]
    @?=
    Right [Para [Str "a", Space, Str "b"]]
  , testCase "whitespace after paragraphs is dropped" $
    runParser (blocks <* eof)
      [Plain "a", Par, White, Plain "b"]
    @?=
    Right [Para [Str "a"], Para [Str "b"]]
  ]

-------------------- examples

-- >> hello world!
example1 :: TeX
example1 = [ Plain "hello"
           , White
           , Plain "world!"
           ]

-- >> {hello} world{!}
example2 :: TeX
example2 = [ Group "" [] [Plain "hello"]
           , White
           , Plain "world"
           , Group "" [] [Plain "!"]
           ]

-- >> \emph{hello} world{!}
example3 :: TeX
example3 = [ Command "emph" [OblArg [Plain "hello"]]
           , White
           , Plain "world"
           , Group "" [] [Plain "!"]
           ]

-- simple "emph" command
-- >> \emph{hello} world
example4 :: TeX
example4 = [ Command "emph" [OblArg [Plain "hello"]]
           , White
           , Plain "world"
           ]

-- multiple TeXAtoms in mandarg of "emph" cmd
-- >> \emph{one two} three
example5 :: TeX
example5 = [ Command "emph" [OblArg [Plain "one", White, Plain "two"]]
           , White
           , Plain "three"
           ]

-- font switch "em" (without enclosing group)
-- >> \em one two
example6 :: TeX
example6 = [ Command "em" []
           , Plain "one"
           , White
           , Plain "two"
           ]

-- font switch "em" (with enclosing group)
-- >> {\em one two} three
example7 :: TeX
example7 = [ Group "" []
             [ Command "em" []
             , Plain "one"
             , White
             , Plain "two"
             ]
           , White
           , Plain "three"
           ]

-- font switches "em" and "rm"
-- >> \em one\rm two\em three
example8 :: TeX
example8 = [ Command "em" []
           , Plain "one"
           , Command "rm" []
           , Plain "two"
           , Command "em" []
           , Plain "three"
           ]

-- itemize list
-- >>:
{-
  pre-list
  \begin{itemize}
  \item one one
  \par
  \item two
  \item three
  \end{itemize}
  after list
-}
exampleList1 :: TeX
exampleList1 = [ Plain "pre-list"
               , White
               , Group "itemize" []
                 [ Command "item" []
                 , Plain "one"
                 , White
                 , Plain "one"
                 , Par
                 , Command "item" []
                 , Plain "two"
                 , White
                 , Command "item" []
                 , Plain "three"
                 ]
               , Plain "after"
               , White
               , Plain "list"
               ]

-- nested list
-- >>:
{-
  pre-list
  \begin{itemize}
    \item up-one\par
    \item up-two
    \begin{itemize}
      \item down-one
      \item down-two
    \end{itemize}
    \item up-three
  \end{itemize}
  after list
-}
exampleList2 :: TeX
exampleList2 = [ Plain "pre-list"
               , White
               , Group "itemize" []
                 [ Command "item" []
                 , Plain "up-one"
                 , Par
                 , Command "item" []
                 , Plain "up-two"
                 , White
                 , Group "itemize" []
                   [ Command "item" []
                   , Plain "down-one"
                   , White
                   , Command "item" []
                   , Plain "down-two"
                   , White
                   ]
                 , Command "item" []
                 , Plain "up-three"
                 ]
               , Plain "after"
               , White
               , Plain "list"
               ]
