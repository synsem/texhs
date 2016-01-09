----------------------------------------------------------------------
-- |
-- Module      :  Text.Doc
-- Copyright   :  2015-2016 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
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
    -- * HTML Writer
  , doc2html
    -- * XML Writer
  , doc2xml
  ) where

import Text.Bib (BibDB)
import Text.Doc.Types (Doc)
import Text.Doc.Reader.TeX (tex2doc)
import Text.Doc.Filter.Bib (docBibFilter, distributeMeta)
import Text.Doc.Writer.Html (doc2html)
import Text.Doc.Writer.Xml (doc2xml)
import Text.TeX.Parser.Types (TeX)


-- | Convert a named 'TeX' AST to a 'Doc' document
-- and resolve citations against a bibliographic database.
tex2docWithBib :: Maybe BibDB -> String -> TeX -> Doc
tex2docWithBib Nothing name input =
  distributeMeta (tex2doc name input)
tex2docWithBib (Just db) name input =
  distributeMeta (docBibFilter db (tex2doc name input))
