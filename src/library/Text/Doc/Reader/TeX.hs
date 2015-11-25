----------------------------------------------------------------------
-- |
-- Module      :  Text.Doc.Reader.TeX
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  portable
--
-- TeX reader: Convert TeX AST to Doc.
----------------------------------------------------------------------

module Text.Doc.Reader.TeX
 ( -- * TeX to Doc Conversion
   tex2doc
 , tex2inlines
   -- * Document parsers
 , doc
 , docmeta
 , docbody
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

import Text.TeX.Context
import Text.TeX.Parser.Types
import Text.Doc.Types


---------- main: TeX to Doc conversion

-- | Convert a 'TeX' document to a 'Doc' document.
tex2doc :: String -> TeX -> Doc
tex2doc name input = case parse doc name input of
  Left l -> error (show l)
  Right r -> r

-- | Parse a list of 'Inline' elements from a 'TeX' AST.
--
-- This can be used to evaluate small bits of TeX code,
-- e.g. BibTeX field values.
tex2inlines :: TeX -> [Inline]
tex2inlines input = case runParser (inlines <* eof) input of
  Left l -> error (show l)
  Right r -> normalizeInlines r

-- Run 'Doc' parser on a 'TeX' structure.
parse :: Parser Doc -> String -> TeX -> ThrowsError Doc
parse p _ input = runParser p input


---------- top-level parsers

-- | Main 'Doc' parser.
-- Read document metadata followed by content.
doc :: Parser Doc
doc = Doc <$> docmeta <*> docbody

-- | Parse meta information from a LaTeX preamble.
docmeta :: Parser Meta
docmeta = many skipWhite *> many (lexemeBlock preambleElem)

-- Parse a single LaTeX preamble element.
preambleElem :: Parser (MetaKey, MetaValue)
preambleElem = choice [documentclass, title, author, usepkg]

-- | Parse content from a LaTeX document body.
docbody :: Parser Content
docbody = grpDown "document" *> dropParents *> content

-- Parse document content.
content :: Parser Content
content = many skipWhite *> blocks <* eof


---------- metainfo parsers

-- Parse document class.
documentclass :: Parser (MetaKey, MetaValue)
documentclass = getCmdBody "documentclass"

-- Parse document title.
title :: Parser (MetaKey, MetaValue)
title = getCmdBody "title"

-- Parse document authors.
author :: Parser (MetaKey, MetaValue)
author = getCmdBody "author"

-- Parse package imports.
usepkg :: Parser (MetaKey, MetaValue)
usepkg = getCmdBodyAs "package" "usepackage"

-- Parse the first mandatory argument of a command @texname@ as a string
-- and pair it with the provided label @keyname@.
getCmdBodyAs :: String -> String -> Parser (MetaKey, MetaValue)
getCmdBodyAs keyname texname =
  ((,) keyname . concatMap plain) <$> (inCmd texname inlines)

getCmdBody :: String -> Parser (MetaKey, MetaValue)
getCmdBody name = getCmdBodyAs name name


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
inline = choice [space, str, emph, em, rm]

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
