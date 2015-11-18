----------------------------------------------------------------------
-- |
-- Module      :  Text.Doc
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Doc Readers and Writers.
----------------------------------------------------------------------

module Text.Doc
  ( -- * Doc Type
    Doc
    -- * TeX Reader
  , tex2doc
    -- * HTML Writer
  , doc2html
    -- * XML Writer
  , doc2xml
  ) where

import Text.Doc.Types (Doc)
import Text.Doc.Reader.TeX (tex2doc)
import Text.Doc.Writer.Html (doc2html)
import Text.Doc.Writer.Xml (doc2xml)
