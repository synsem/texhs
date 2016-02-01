{-# LANGUAGE OverloadedStrings #-}
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
 , figure
 , table
 , tabular
   -- * Inline parsers
 , inlines
 , inline
 , space
 , str
 , emph
 , em
 , rm
   -- * Number parsers
 , number
   -- * Text parsers
 , literalText
 , literalString
   -- * Unit parsers
 , skipSpace
 , skipSpaces
 , skipPar
 , skipPars
 , skipInterlevel
 ) where


import Control.Applicative
import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (readDec)

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
preamble = skipPars <* many (lexemeBlock preambleElem)

-- Parse a single LaTeX preamble element.
preambleElem :: Parser ()
preambleElem = choice [title, authors, date, usepkg, documentclass]

-- | Parse content from a LaTeX document body.
docbody :: Parser Content
docbody = grpDown "document" *> dropParents *> content

-- Parse document content.
content :: Parser Content
content = skipPars *> blocks <* eof


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
lexeme p = p <* skipSpaces

-- Combinator that drops any trailing whitespace ('White'),
-- paragraph breaks ('Par') and void inter-level elements.
lexemeBlock :: Parser a -> Parser a
lexemeBlock p = p <* skipPars


---------- Block parsers

-- | Parse any number of blocks.
blocks :: Parser [Block]
blocks = many (lexemeBlock block)

-- | Parse a single block.
block :: Parser Block
block = choice
  [ header, itemize, enumerate, quotation
  , itemblock, igt, figure, table, para]

-- Parse block elements in the first mandatory argument of a command.
blockCmd :: String -> Parser [Block]
blockCmd name = inCmd name blocks

-- | Parse a single (non-empty) paragraph.
para :: Parser Block
para = Para <$> someInlines <* skipPars

-- | Parse a chapter or section heading.
header :: Parser Block
header = do
  (level', (starred', title')) <- choice headerParsers
  anchor' <- if starred'
             then modifyMeta registerHeaderPhantom *>
                  (getPhantomAnchor <$> getMeta)
             else modifyMeta (registerHeader level') *>
                  (metaAnchorCurrent <$> getMeta)
  skipPars -- may contain label assignments, thus must follow 'modifyMeta'
  return (Header level' anchor' title')
  where
    headerParsers = map parseHeader headerMap ++
                    map parseKomaHeader komaHeaderMap
    -- (1) parse standard sectioning commands from LaTeX book class
    parseHeader (headerLevel, headerCmdName) =
      (,) headerLevel <$> inlineCmdCheckStar headerCmdName
    headerMap = zip [1..]
      [ "part", "chapter", "section", "subsection"
      , "subsubsection", "paragraph", "subparagraph"]
    -- (2) parse sectioning commands from KOMA-script classes:
    --     they all introduce unnumbered sections,
    --     so we are faking a 'StarArg' for them.
    parseKomaHeader (headerLevel, headerCmdName) =
      ((,) headerLevel . (,) True) <$> inlineCmd headerCmdName
    komaHeaderMap = zip [1..] ["addpart", "addchap", "addsec"]

-- | Parse an @itemize@ group.
itemize :: Parser Block
itemize = List UnorderedList <$>
  inGrp "itemize" (skipPars *> list (lexemeBlock (cmd "item")) blocks)

-- | Parse an @enumerate@ group.
enumerate :: Parser Block
enumerate = List OrderedList <$>
  inGrp "enumerate" (skipPars *> list (lexemeBlock (cmd "item")) blocks)

-- | Parse a @quotation@ group.
quotation :: Parser Block
quotation = QuotationBlock <$>
  inGrp "quotation" (skipPars *> blocks)

-- Note: For simplicity, we allow top-level @xlist@ groups.
-- They are numbered relative to the most recent @ex@ item.
--
-- | Parse an @exe@ or @xlist@ group of list items
-- that are numbered relative to a global counter.
--
-- (See @gb4e@ package for LaTeX.)
itemblock :: Parser Block
itemblock = lexemeBlock $ choice
  [ inGrp "exe" itemblockInner
  , inGrp "xlist" (withItemSubList itemblockInner)]

-- Parse the body of an @exe@ or @xlist@ group.
itemblockInner :: Parser Block
itemblockInner = skipPars *>
  (ListItemBlock <$> many (lexemeBlock exItem))

-- Parse an @ex@ command (in an @exe@ or @xlist@ group)
-- as an item in a 'ListItemBlock'.
exItem :: Parser ListItem
exItem = do
  anchor' <- cmd "ex" *> lexemeBlock registerListItem
  ListItem anchor' <$> blocks

-- Register listitem as the current anchor
-- (e.g. for subsequent @label@ assignments).
registerListItem :: Parser InternalAnchor
registerListItem = do
  meta <- getMeta
  let (ch, itemCntOld) = metaItemCurrent meta
  let itemCnt = case itemCntOld of
        (x:xs) -> x+1:xs -- increment item count at current level
        [] -> error "registerListItem: encountered invalid (empty) item stack"
  putMeta (meta { metaItemCurrent = (ch, itemCnt)
                , metaAnchorCurrent = ItemAnchor (ch, itemCnt) })
  return (ItemAnchor (ch, itemCnt))

-- Run parser in an environment with an additional sublist layer.
withItemSubList :: Parser a -> Parser a
withItemSubList p = pushItemSubList *> p <* popItemSubList
  where
    -- Push a sublist layer to the current listitem counter
    -- and initialize it with zero.
    pushItemSubList :: Parser ()
    pushItemSubList = modifyMeta $ \meta ->
      meta { metaItemCurrent = (0:) <$> metaItemCurrent meta }
    -- Pop a sublist layer from the current listitem counter.
    popItemSubList :: Parser ()
    popItemSubList = modifyMeta $ \meta ->
      let newItemCurrent = case metaItemCurrent meta of
            (ch, _:xs@(_:_)) -> (ch, xs)
            (_, [_]) -> error "withItemSubList: encountered invalid (singleton) item stack"
            (_, []) -> error "withItemSubList: encountered invalid (empty) item stack"
      in meta { metaItemCurrent = newItemCurrent }

-- | Parse interlinear glossed text (igt)
-- following a @gll@ command (from @gb4e@ package)
-- with an optional trailing @glt@ or @trans@ line.
igt :: Parser Block
igt = do
  let linerange = [2..8] -- allowed number of aligned lines in an igt block
  numlines <- choice (map (\n ->
    n <$ lexemeBlock (cmd ('g' : replicate n 'l'))) linerange)
  alignedlines <- count numlines glossline
  maybetrans <- optional ((cmd "glt" <|> cmd "trans") *> inlines)
  let cols = maximum (map length alignedlines)
      wraptrans t = [[MultiCell cols t]]
      translation = maybe [] wraptrans maybetrans
  return (SimpleTable (alignedlines ++ translation))

-- Parse a single line of words (to be aligned),
-- separated by whitespace.
glossline :: Parser TableRow
glossline =
  let word = concat <$> some (some inlineWord <|> inGrp "" inlines)
      eol = lexemeBlock (satisfy isNewline)
  in (SingleCell <$> word) `sepEndBy` lexeme space <* eol

-- | Parse a @figure@ group.
--
-- This group is required to contain the commands
-- @includegraphics@ (from @graphicx@ package) and @caption@.
figure :: Parser Block
figure = inGrp "figure" $ do
  anchor <- registerFigure
  skipPars <* optional (lexemeBlock (choice
    [ void (cmd "centering")
    , grpUnwrap "center"]))
  imgloc <- lexemeBlock (literalTextArg "includegraphics")
  imgdesc <- lexemeBlock (inlineCmd "caption")
  return (Figure anchor imgloc imgdesc)

-- Register figure as the current anchor
-- (e.g. for subsequent @label@ assignments).
registerFigure :: Parser InternalAnchor
registerFigure = do
  meta <- getMeta
  let figCnt = (+1) <$> metaFigureCurrent meta
  putMeta (meta { metaFigureCurrent = figCnt
                , metaAnchorCurrent = FigureAnchor figCnt })
  return (FigureAnchor figCnt)

-- | Parse a @table@ group.
--
-- This group is required to contain a @tabular@ group
-- and a @caption@ command.
table :: Parser Block
table = inGrp "table" $ do
  anchor <- registerTable
  skipPars <* optional (lexemeBlock (choice
    [ void (cmd "centering")
    , grpUnwrap "center"]))
  tabledata <- lexemeBlock tabular
  tabledesc <- lexemeBlock (inlineCmd "caption")
  return (Table anchor tabledesc tabledata)

-- | Parse a @tabular@ group
tabular :: Parser [TableRow]
tabular = inGrp "tabular" $ do
  let sep = lexeme (satisfy isAlignMark)      -- cell separator: "&"
      eol = lexemeBlock (satisfy isNewline)   -- end of row: "\\\\"
      hline = void $ lexemeBlock (cmd "hline")
      tableRow = tableCell `sepBy` sep <* eol <* many hline
  many (skipSpace <|> hline) *> many tableRow

-- Parse a table cell in a @tabular@ group.
tableCell :: Parser TableCell
tableCell = choice
  [ (uncurry MultiCell . (\(n,_,d) -> (n,d))) <$>
    cmdThreeOblArgs "multicolumn" (fromMaybe 1 <$> number) inlines inlines
  , SingleCell <$> inlines]

-- Register table as the current anchor
-- (e.g. for subsequent @label@ assignments).
registerTable :: Parser InternalAnchor
registerTable = do
  meta <- getMeta
  let tableCnt = (+1) <$> metaTableCurrent meta
  putMeta (meta { metaTableCurrent = tableCnt
                , metaAnchorCurrent = TableAnchor tableCnt })
  return (TableAnchor tableCnt)


---------- Inline parsers

-- | Parse a single inline element.
inline :: Parser Inline
inline = space <|> inlineWord

-- Parse a non-space inline element.
inlineWord :: Parser Inline
inlineWord = choice
  [ str, emph, em, rm
  , cite, note, ref, href, url
  , math ]

-- | Parse any number of inline elements.
--
-- Anonymous groups are flattened.
inlines :: Parser [Inline]
inlines = concat <$> many (choice
  [ count 1 inline
  , inGrp "" inlines
  , [] <$ skipInterlevel ])

-- | Parse a non-empty list of inline elements.
--
-- Anonymous groups are flattened.
someInlines :: Parser [Inline]
someInlines = choice
  [ (:) <$> inline <*> inlines
  , (++) <$> inGrp "" someInlines <*> inlines ]

-- Parse content of a math group.
inlinesMath :: Parser [Inline]
inlinesMath = concat <$> many (choice
  [ count 1 inlineMath
  , inGrp "" inlines
  , [] <$ skipInterlevel ])

-- | Parse a single inline element in a math group.
inlineMath :: Parser Inline
inlineMath = sup <|> sub <|> inline

-- Parse inline elements in the first mandatory argument of a command.
inlineCmd :: String -> Parser [Inline]
inlineCmd name = inCmd name inlines

-- Parse inline elements in the first mandatory argument of a command
-- and also return a flag that indicates whether the command was starred.
inlineCmdCheckStar :: String -> Parser (Bool, [Inline])
inlineCmdCheckStar name = inCmdCheckStar name inlines

-- Parse the first mandatory argument of a command as raw text.
literalTextArg :: String -> Parser Text
literalTextArg name = inCmd name literalText

-- | Parse whitespace.
space :: Parser Inline
space = Space <$ lexeme (satisfy isWhite)

-- Parse math group (to unicode representation).
math :: Parser Inline
math = uncurry Math <$> inMathGrp inlinesMath

-- Parse superscript.
sup :: Parser Inline
sup = FontStyle Sup <$> inSupScript inlinesMath

-- Parse subscript.
sub :: Parser Inline
sub = FontStyle Sub <$> inSubScript inlinesMath

-- | Parse character data.
str :: Parser Inline
str = Str <$> literalPlain

-- | Parse @emph@ command.
emph :: Parser Inline
emph = FontStyle Emph <$> inlineCmd "emph"

-- | Parse @em@ command.
em :: Parser Inline
em = FontStyle Emph <$> (cmd "em" *> inlines)

-- | Parse @rm@ command.
rm :: Parser Inline
rm = FontStyle Normal <$> (cmd "rm" *> inlines)

-- | Parse @cite@ command.
cite :: Parser Inline
cite = do
  (citemode, keys) <- choice citationParsers
  modifyMeta (registerCiteKeys keys)
  let mcite = MultiCite citemode [] [] [SingleCite [] [] keys]
  return (Citation mcite Nothing)
  where
    citationParsers :: [Parser (CiteMode, [CiteKey])]
    citationParsers = map
      (\(name, mode) -> (,) mode <$> (parseCiteKeys <$> literalTextArg name))
      (M.assocs citeCommandMap)

-- Citation modes associated with known citation commands.
citeCommandMap :: Map String CiteMode
citeCommandMap = M.fromList
  [ -- biblatex
    ("cite", CiteBare)
  , ("parencite", CiteParen)
  , ("textcite", CiteText)
  , ("citeauthor", CiteAuthor)
  , ("citeyear", CiteYear)
    -- natbib
  , ("citet", CiteText)
  , ("citep", CiteParen)
  , ("citealt", CiteBare)
  , ("citealp", CiteBare)
  ]

-- Extract CiteKeys from the obligatory argument of a citation command.
parseCiteKeys :: Text -> [CiteKey]
parseCiteKeys =
  filter (not . T.null) .
  map T.strip . T.split (==',')

-- | Parse @footnote@ command.
note :: Parser Inline
note = do
  fntext <- blockCmd "footnote"
  anchor <- registerNote fntext
  return (Note anchor fntext)

-- Register new footnote: assign number and store footnote text
-- in the document meta information.
registerNote :: [Block] -> Parser InternalAnchor
registerNote fntext = do
  meta <- getMeta
  let newCnt = (+1) <$> metaNoteCurrent meta
      newMap = M.insert newCnt fntext (metaNoteMap meta)
  putMeta (meta { metaNoteCurrent = newCnt
                , metaNoteMap = newMap })
  return (NoteAnchor newCnt)

-- | Parse @ref@ command.
ref :: Parser Inline
ref = flip Pointer Nothing <$> literalTextArg "ref"

-- | Parse @href@ command from @hyperref@ package.
href :: Parser Inline
href = do
  (url', description) <- cmdTwoOblArgs "href" literalText inlines
  return (Pointer "external" (Just (ExternalResource description url' "" "")))

-- | Parse @url@ command from @hyperref@ package.
url :: Parser Inline
url = do
  url' <- literalTextArg "url"
  return (Pointer "external" (Just (ExternalResource [Str (T.unpack url')] url' "" "")))


---------- Number parsers

-- | Parse a decimal number.
number :: Parser (Maybe Int)
number = parseDigits <$> literalString
  where
    parseDigits s = case readDec s of
      [(i,[])] -> Just i
      _ -> Nothing


---------- Text parsers

-- | Parse content as raw text.
--
-- See 'literalString'.
literalText :: Parser Text
literalText = T.pack <$> literalString

-- | Parse content as raw string.
--
-- This will deconstruct subscripts and superscripts
-- as well as whitespace and walk into groups.
literalString :: Parser String
literalString = concat <$> many (choice
  [ literalPlain
  , literalGroup
  , literalSub
  , literalSup
  , literalAlign
  , literalSpace
  ])

-- Extract string from 'Plain' atom.
literalPlain :: Parser String
literalPlain = satisfy isPlain >>= \(Plain xs) -> return xs

-- Extract string content from an anonymous group.
literalGroup :: Parser String
literalGroup = inGrp "" literalString

-- Deconstruct 'SubScript' atom to raw text
-- with underscore (\'_\') prefix.
literalSub :: Parser String
literalSub = ('_':) <$> inSubScript literalString

-- Deconstruct 'SupScript' atom to raw text
-- with circumflex (\'^\') prefix.
literalSup :: Parser String
literalSup = ('^':) <$> inSupScript literalString

-- Deconstruct 'AlignMark' atom to \'&\'.
literalAlign :: Parser String
literalAlign = "&" <$ satisfy isAlignMark

-- Deconstruct 'White' atom to normal space (\" \").
literalSpace :: Parser String
literalSpace = " " <$ skipSpace


---------- Unit parsers: void inter-level elements

-- | Skip a single inter-level element.
skipInterlevel :: Parser ()
skipInterlevel = choice [label, bookregion, nocite]

-- | Parse @label@ command.
label :: Parser ()
label = do
  label' <- literalTextArg "label"
  modifyMeta (registerAnchorLabel label')

-- | Parse a command that affects the current bookregion:
-- @frontmatter@, @mainmatter@, @appendix@ or @backmatter@.
bookregion :: Parser ()
bookregion = choice (map parseRegion regionMap)
  where
    parseRegion (regionCmdName, regionType) =
      cmd regionCmdName *> modifyMeta (registerRegion regionType)
    regionMap = [ ("frontmatter", Frontmatter)
                , ("mainmatter", Mainmatter)
                , ("appendix", Backmatter)
                , ("backmatter", Backmatter)]

-- | Parse @nocite@ command.
nocite :: Parser ()
nocite = do
  keys <- parseCiteKeys <$> literalTextArg "nocite"
  modifyMeta (registerCiteKeys keys)


---------- Unit parsers: whitespace

-- | Skip any inline-level whitespace,
-- including inter-level elements.
skipSpaces :: Parser ()
skipSpaces = void $ many (choice [skipSpace, skipInterlevel])

-- | Skip any block-level whitespace,
-- including inter-level elements.
skipPars :: Parser ()
skipPars = void $ many (choice [skipPar, skipSpace, skipInterlevel])

-- | Skip a single whitespace element ('White').
skipSpace :: Parser ()
skipSpace = void $ satisfy isWhite

-- | Skip a single paragraph break ('Par').
skipPar :: Parser ()
skipPar = void $ satisfy isPar
