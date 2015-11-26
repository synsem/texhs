{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.Bib.Writer
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  portable
--
-- BibTeX formatter.
----------------------------------------------------------------------

module Text.Bib.Writer
  ( getCiteAgent
  , getCiteYear
  , getCiteFull
  ) where

import Control.Applicative

import Text.Bib.Types
import Text.Doc.Types


-------------------- Formatting

-- | Construct author part of an author-year citation.
getCiteAgent :: BibEntry -> [Inline]
getCiteAgent entry =
  let authors = maybe [] id (getBibAgents "author" entry <|> getBibAgents "editor" entry)
      nrAuthors = length authors
      sepInner = [Str ",", Space]
      sepFinal = [Space, Str "&", Space]
      sepInners = replicate (max 0 (nrAuthors - 2)) sepInner
      sepFinals = if nrAuthors > 1 then (sepFinal:[]) else [[]]
      fillers = sepInners ++ sepFinals
  in concat $ zipWith (++) (map agentLast authors) fillers

-- | Construct year part of an author-year citation.
getCiteYear :: BibEntry -> [Inline]
getCiteYear = maybe [] id . getBibField "year"

-- | Construct full bibliographic reference for an entry.
getCiteFull :: BibEntry -> [Inline]
getCiteFull entry =
  getCiteAgent entry ++ [Space] ++
  getCiteYear entry ++ [Space] ++
  maybe [] id (getBibField "title" entry) ++
  [Str "."]
