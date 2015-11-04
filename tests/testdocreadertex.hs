----------------------------------------------------------------------
-- Tests for the @Text.Doc.Reader.TeX@ module.
----------------------------------------------------------------------
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
-- Maintainer  :  mathias.schenner@langsci-press.org
----------------------------------------------------------------------

module Main where

import System.Exit (ExitCode, exitSuccess, exitFailure)
import Test.HUnit (Test(..), Counts(..), test, (~:), (~=?), runTestTT)

import Text.TeX.Context.Types
import Text.TeX.Context.Walk
import Text.TeX.Parser.Types
import Text.Doc.Types
import Text.Doc.Reader.TeX


-------------------- tests

testsBasic :: Test
testsBasic = TestLabel "basic traversals" $ test
  [ "item"
    ~: Right (Plain "hello")
    ~=? runParser item example1
  , "chained item"
    ~: Right (Plain "world!")
    ~=? runParser (item >> item >> item) example1
  , "failing chained item"
    ~: Left [EndOfGroup]
    ~=? runParser (item >> item >> item >> item) example1
  , "satisfy"
    ~: Right (Plain "hello")
    ~=? runParser (satisfy isPlain) example1
  , "failing satisfy"
    ~: Left [Unexpected White]
    ~=? runParser (item >> satisfy isPlain) example1
  , "walk into group"
    ~: Right (Plain "hello")
    ~=? runParser (goDown >> item) example2
  , "failing item inside of group"
    ~: Left [EndOfGroup]
    ~=? runParser (goDown >> item >> item) example2
  , "enter and leave group"
    ~: Right White
    ~=? runParser (goDown >> item >> goUp >> item) example2
  ]

testsInlines :: Test
testsInlines = TestLabel "inline elements" $ test
  [ "simple emph"
    ~: Right (Emph [Str "hello"])
    ~=? runParser emph example5
  , "emph with inner space"
    ~: Right (Emph [Str "one",Space,Str "two"])
    ~=? runParser emph example6
  , "return to parent after emph"
    ~: Right (Plain "world")
    ~=? runParser (emph *> space *> item) example5
  , "em with inner space"
    ~: Right (Emph [Str "one",Space,Str "two"])
    ~=? runParser em example7
  , "failing em"
    ~: Left [Unexpected (Group "" []
             [ Command "em" [], Plain "one", White, Plain "two"])]
    ~=? runParser em example8
  , "nested em"
    ~: Right (Emph [Str "one",Space,Str "two"])
    ~=? runParser (optNested em) example8
  , "combining nested em with parent inlines"
    ~: Right [Emph [Str "one",Space,Str "two"],Space,Str "three"]
    ~=? runParser ((:) <$> optNested em <*> inlines) example8
  , "rm between em font switches"
    ~: Right [Emph [Str "one",Normal [Str "two",Emph [Str "three"]]]]
    ~=? runParser inlines example9
  ]

testsLists :: Test
testsLists = TestLabel "list blocks" $ test
  [ "simple list"
    ~: Right (List [[Para [Str "one",Space,Str "one"]]
                   ,[Para [Str "two",Space]]
                   ,[Para [Str "three"]]])
    ~=? runParser (inlines *> itemize) exampleList1
  , "nested list"
    ~: Right (List [[Para [Str "up-one"]]
                   ,[Para [Str "up-two",Space]
                    ,List [[Para [Str "down-one",Space]]
                          ,[Para [Str "down-two",Space]]]]
                   ,[Para [Str "up-three"]]])
    ~=? runParser (inlines *> itemize) exampleList2
  ]


-- collect all tests
tests :: Test
tests = TestList
  [ testsBasic
  , testsInlines
  , testsLists
  ]

-- run tests
main :: IO ExitCode
main = do
  counts <- runTestTT tests
  if errors counts == 0 && failures counts == 0
    then exitSuccess
    else exitFailure


-------------------- examples

-- >> hello world!
example1 :: TeX
example1 = [Plain "hello", White, Plain "world!"]

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

-- >> H_2{O}
example4 :: TeX
example4 = [ Plain "H"
           , SubScript [Plain "2"]
           , Group "" [] [Plain "O"]
           ]

-- simple "emph" command
-- >> \emph{hello} world
example5 :: TeX
example5 = [ Command "emph" [OblArg [Plain "hello"]]
           , White
           , Plain "world"
           ]

-- multiple TeXAtoms in mandarg of "emph" cmd
-- >> \emph{one two} three
example6 :: TeX
example6 = [ Command "emph" [OblArg [Plain "one", White, Plain "two"]]
           , White
           , Plain "three"
           ]

-- font switch "em" (without enclosing group)
-- >> \em one two
example7 :: TeX
example7 = [ Command "em" []
           , Plain "one"
           , White
           , Plain "two"
           ]

-- font switch "em" (with enclosing group)
-- >> {\em one two} three
example8 :: TeX
example8 = [ Group "" []
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
example9 :: TeX
example9 = [ Command "em" []
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
