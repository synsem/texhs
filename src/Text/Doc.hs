----------------------------------------------------------------------
-- |
-- Module      :  Text.Doc
-- Copyright   :  2015-2017 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
--
-- Maintainer  :  mschenner.dev@gmail.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- Doc Readers and Writers.
----------------------------------------------------------------------

module Text.Doc
  ( -- * Doc Type
    Doc
    -- * TeX Reader
  , tex2doc
  , tex2docWithBib
    -- * EPUB Writer
  , doc2epub
    -- * HTML Writer
  , doc2html
  , doc2multiHtml
    -- * XML Writer
  , doc2xml
  ) where

import Text.Bib (BibDB)
import Text.Doc.Types (Doc)
import Text.Doc.Reader.TeX (tex2doc)
import Text.Doc.Filter.Bib (docBibFilter)
import Text.Doc.Writer.Epub (doc2epub)
import Text.Doc.Writer.Html (doc2html, doc2multiHtml)
import Text.Doc.Writer.Xml (doc2xml)
import Text.TeX.Parser.Types (TeX)


-- | Convert a named 'TeX' AST to a 'Doc' document
-- and resolve citations against a bibliographic database.
tex2docWithBib :: Maybe BibDB -> String -> TeX -> Doc
tex2docWithBib Nothing name input =
  tex2doc name input
tex2docWithBib (Just db) name input =
  docBibFilter db (tex2doc name input)
