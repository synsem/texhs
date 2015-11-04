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
  ( -- * TeX Reader
    tex2doc
  ) where

import Text.Doc.Reader.TeX (tex2doc)