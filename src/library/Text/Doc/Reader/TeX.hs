----------------------------------------------------------------------
-- |
-- Module      :  Text.Doc.Reader.TeX
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  GHC
--
-- TeX reader: Convert TeX AST to Doc.
----------------------------------------------------------------------

module Text.Doc.Reader.TeX
 ( -- * TeX to Doc Conversion
   tex2doc
 , tex2inlines
   -- * Document parser
 , doc
   -- * Block parsers
 , blocks
 , block
 , para
 , header
 , itemize
   -- * Inline parsers
 , inlines
 , inline
 , space
 , str
 , emph
 , em
 , rm
 ) where


import Control.Applicative
import Control.Monad
import qualified Data.Set as Set
import qualified Data.Text as T

import Text.TeX.Context
import Text.TeX.Parser.Types
import Text.Doc.Types


---------- main: TeX to Doc conversion

-- | Convert a named 'TeX' AST to a 'Doc' document.
tex2doc :: String -> TeX -> Doc
tex2doc name input =
  case runParserWithState doc input of
    Left l -> error (name ++ ": " ++ show l)
    Right (c,m) -> Doc m c

-- | Parse a list of 'Inline' elements from a 'TeX' AST.
--
-- This can be used to evaluate small bits of TeX code,
-- e.g. BibTeX field values.
tex2inlines :: TeX -> [Inline]
tex2inlines input =
  case runParser (inlines <* eof) input of
    Left l -> error (show l)
    Right r -> normalizeInlines r


---------- top-level parsers

-- | Main document parser.
--
-- Parse LaTeX preamble followed by document body.
doc :: Parser Content
doc = preamble *> docbody

-- | Parse meta information from a LaTeX preamble.
preamble :: Parser ()
preamble = void $ many skipWhite *> many (lexemeBlock preambleElem)

-- Parse a single LaTeX preamble element.
preambleElem :: Parser ()
preambleElem = choice [title, authors, date, usepkg, documentclass]

-- | Parse content from a LaTeX document body.
docbody :: Parser Content
docbody = grpDown "document" *> dropParents *> content

-- Parse document content.
content :: Parser Content
content = many skipWhite *> blocks <* eof


---------- metainfo parsers

-- Parse document class.
documentclass :: Parser ()
documentclass = void $ getCmdBody "documentclass"

-- Parse package imports.
usepkg :: Parser ()
usepkg = void $ getCmdBody "usepackage"

-- Parse document title.
title :: Parser ()
title = do
  title' <- getCmdBody "title"
  meta <- getMeta
  putMeta (meta { metaTitle = title' })

-- Parse document authors.
authors :: Parser ()
authors = do
  authors' <- getCmdBody "author"
  meta <- getMeta
  putMeta (meta { metaAuthors = [authors'] })

-- Parse document date.
date :: Parser ()
date = do
  date' <- getCmdBody "date"
  meta <- getMeta
  putMeta (meta { metaDate = date' })

-- Parse the first mandatory argument of a specific command.
getCmdBody :: String -> Parser [Inline]
getCmdBody name = inCmd name inlines


---------- combinators

-- Combinator that drops trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme p = p <* many skipSpace

-- Combinator that drops any trailing 'White' and 'Par'.
lexemeBlock :: Parser a -> Parser a
lexemeBlock p = p <* many skipWhite


---------- Block parsers

-- | Parse any number of blocks.
blocks :: Parser [Block]
blocks = many (lexeme block)

-- | Parse a single block.
block :: Parser Block
block = choice [header, itemize, para]

-- | Parse a single (non-empty) paragraph.
para :: Parser Block
para = Para <$> (some inline <* optional skipPar)

-- | Parse a chapter or section heading.
header :: Parser Block
header = choice
         [ Header 1 <$> inlineCmd "part"
         , Header 2 <$> inlineCmd "chapter"
         , Header 3 <$> inlineCmd "section"
         , Header 4 <$> inlineCmd "subsection"
         , Header 5 <$> inlineCmd "subsubsection"
         ] <* skipWhite

-- | Parse an @itemize@ group.
itemize :: Parser Block
itemize = List <$> (inGrp "itemize" (list (cmd "item") blocks))


---------- Inline parsers

-- | Parse any number of inline elements.
--
-- Anonymous groups are flattened.
inlines :: Parser [Inline]
inlines = concat <$> (many (count 1 inline <|> inGrp "" inlines))

-- | Parse a single inline element.
inline :: Parser Inline
inline = choice [space, str, emph, em, rm, cite]

-- Parse inline elements in the first mandatory argument of a command.
inlineCmd :: String -> Parser [Inline]
inlineCmd name = inCmd name inlines

-- | Parse whitespace.
space :: Parser Inline
space = Space <$ satisfy isWhite

-- | Parse character data.
str :: Parser Inline
str = Str <$> (satisfy isPlain >>= \(Plain xs) -> return xs)

-- | Parse @emph@ command.
emph :: Parser Inline
emph = Emph <$> (inCmd "emph" inlines)

-- | Parse @em@ command.
em :: Parser Inline
em = Emph <$> (cmd "em" *> inlines)

-- | Parse @rm@ command.
rm :: Parser Inline
rm = Normal <$> (cmd "rm" *> inlines)

-- | Parse @cite@ command.
cite :: Parser Inline
cite = do
  arg <- inlineCmd "cite"
  let keys = (T.split (==',') . T.pack . concatMap plain) arg
  meta <- getMeta
  let newCiteSet = foldr Set.insert (metaCites meta) keys
  putMeta (meta { metaCites = newCiteSet })
  return (Citation (MultiCite CiteParen [] [] [SingleCite [] [] keys]))


---------- Unit parsers

-- | Skip a single whitespace element ('White').
skipSpace :: Parser ()
skipSpace = void $ satisfy isWhite

-- | Skip a 'Par' element.
skipPar :: Parser ()
skipPar = void $ satisfy isPar

-- | Skip a 'White' or 'Par' element.
skipWhite :: Parser ()
skipWhite = void $ satisfy (\x -> isPar x || isWhite x)
