----------------------------------------------------------------------
-- |
-- Module      :  Text.Doc.Reader.TeX
-- Copyright   :  2015-2016 Mathias Schenner,
--                2015-2016 Language Science Press.
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
 , enumerate
 , quotation
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


---------- Preamble parsers

-- Parse document class.
documentclass :: Parser ()
documentclass = void $ inlineCmd "documentclass"

-- Parse package imports.
usepkg :: Parser ()
usepkg = void $ inlineCmd "usepackage"

-- Parse document title.
title :: Parser ()
title = do
  title' <- inlineCmd "title"
  meta <- getMeta
  putMeta (meta { metaTitle = title' })

-- Parse document authors.
authors :: Parser ()
authors = do
  authors' <- inlineCmd "author"
  meta <- getMeta
  putMeta (meta { metaAuthors = [authors'] })

-- Parse document date.
date :: Parser ()
date = do
  date' <- inlineCmd "date"
  meta <- getMeta
  putMeta (meta { metaDate = date' })


---------- combinators

-- Combinator that drops trailing whitespace ('White')
-- and void inter-level elements.
lexeme :: Parser a -> Parser a
lexeme p = p <* many (choice [skipSpace, skipInterlevel])

-- Combinator that drops any trailing whitespace ('White'),
-- paragraph breaks ('Par') and void inter-level elements.
lexemeBlock :: Parser a -> Parser a
lexemeBlock p = p <* many (choice [skipWhite, skipInterlevel])


---------- Block parsers

-- | Parse any number of blocks.
blocks :: Parser [Block]
blocks = many (lexeme block)

-- | Parse a single block.
block :: Parser Block
block = choice [header, itemize, enumerate, quotation, para]

-- | Parse a single (non-empty) paragraph.
para :: Parser Block
para = Para <$> ((:) <$> inline <*> inlines <* optional skipPar)

-- | Parse a chapter or section heading.
header :: Parser Block
header = do
  (level', title') <- choice
    [ (,) 1 <$> inlineCmd "part"
    , (,) 2 <$> inlineCmd "chapter"
    , (,) 3 <$> inlineCmd "section"
    , (,) 4 <$> inlineCmd "subsection"
    , (,) 5 <$> inlineCmd "subsubsection"
    ]
  anchor' <- registerHeader level'
  void $ many (choice [skipWhite, skipInterlevel])
  return (Header level' anchor' title')

registerHeader :: Level -> Parser Anchor
registerHeader level = do
  meta <- getMeta
  let sectionCurrent = incSection level (metaSectionCurrent meta)
  putMeta (meta { metaSectionCurrent = sectionCurrent
                , metaAnchorCurrent = SectionAnchor sectionCurrent
                })
  return (SectionAnchor sectionCurrent)

-- | Parse an @itemize@ group.
itemize :: Parser Block
itemize = List UnorderedList <$>
  inGrp "itemize" (list (cmd "item") blocks)

-- | Parse an @enumerate@ group.
enumerate :: Parser Block
enumerate = List OrderedList <$>
  inGrp "enumerate" (list (cmd "item") blocks)

-- | Parse a @quotation@ group.
quotation :: Parser Block
quotation = QuotationBlock <$>
  inGrp "quotation" blocks


---------- Inline parsers

-- | Parse any number of inline elements.
--
-- Anonymous groups are flattened.
inlines :: Parser [Inline]
inlines = concat <$> many (choice
  [ count 1 inline
  , inGrp "" inlines
  , [] <$ skipInterlevel ])

-- | Parse a single inline element.
inline :: Parser Inline
inline = choice [space, str, emph, em, rm, cite, ref]

-- Parse inline elements in the first mandatory argument of a command.
inlineCmd :: String -> Parser [Inline]
inlineCmd name = inCmd name inlines

-- Parse raw textual label in the first mandatory argument of a command.
textLabel :: String -> Parser Label
textLabel name = inCmd name (satisfy isPlain >>= \(Plain xs) -> return (T.pack xs))

-- | Parse whitespace.
space :: Parser Inline
space = Space <$ lexeme (satisfy isWhite)

-- | Parse character data.
str :: Parser Inline
str = Str <$> (satisfy isPlain >>= \(Plain xs) -> return xs)

-- | Parse @emph@ command.
emph :: Parser Inline
emph = Emph <$> inlineCmd "emph"

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
  modifyMeta (registerCiteKeys keys)
  let mcite = MultiCite CiteParen [] [] [SingleCite [] [] keys]
  return (Citation mcite Nothing)

-- | Parse @ref@ command.
ref :: Parser Inline
ref = flip Pointer Nothing <$> textLabel "ref"


---------- Unit parsers: void inter-level elements

-- | Skip a single inter-level element.
skipInterlevel :: Parser ()
skipInterlevel = label

-- | Parse @label@ command.
label :: Parser ()
label = do
  label' <- textLabel "label"
  modifyMeta (registerAnchorLabel label')


---------- Unit parsers: whitespace

-- | Skip a single whitespace element ('White').
skipSpace :: Parser ()
skipSpace = void $ satisfy isWhite

-- | Skip a 'Par' element.
skipPar :: Parser ()
skipPar = void $ satisfy isPar

-- | Skip a 'White' or 'Par' element.
skipWhite :: Parser ()
skipWhite = void $ satisfy (\x -> isPar x || isWhite x)
