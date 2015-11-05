----------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Executable driver for texhs.
----------------------------------------------------------------------

module Main where

import qualified Data.Text.Lazy.IO as T
import System.Environment

import Text.TeX (readTeXIO)
import Text.Doc (tex2doc, doc2html)

main :: IO ()
main = do
  argv <- getArgs
  case argv of
    [filename] -> T.putStrLn =<< (doc2html `fmap` tex2doc filename `fmap`
                                  (readTeXIO filename =<< readFile filename))
    _ -> error "Usage: provide a texfile as single argument"
