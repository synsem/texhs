{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.Bib.Reader.BibTeX.Reference
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  portable
--
-- BibTeX parser, layer 2:
-- interpret field values according to BibLaTeX's data model
-- (also see "Text.Bib.Types"), e.g. extract person names.
-- Cross-references between BibTeX entries are not resolved;
-- see "Text.Bib.Reader.BibTeX.Inheritance" instead.
----------------------------------------------------------------------

module Text.Bib.Reader.BibTeX.Reference
  ( -- * Parsers
    parseBib
  , parseAgents
  , parseList
  ) where

import Data.Char (isLower)
import Data.Maybe (mapMaybe)
import Data.List (partition, intercalate)
import Data.Text (Text)
import qualified Data.Text as T

import Text.Bib.Types
import Text.Bib.Reader.BibTeX.Structure
import Text.Doc.Types
import Text.Doc.Reader.TeX (tex2inlines)
import Text.TeX (readTeX)
import Text.TeX.Parser.Types


-------------------- Config

-- Default delimiter in name lists and literal lists.
nameSep :: String
nameSep = "and"

-- BibTeX fields that contain name lists.
agentFields :: [Text]
agentFields =
  [ "author"
  , "bookauthor"
  , "bookeditor"
  , "editor"
  ]

-- BibTeX fields that contain literal lists.
listFields :: [Text]
listFields =
  [ "institution"
  , "location"
  , "organization"
  , "origlocation"
  , "origpublisher"
  , "publisher"
  ]


-------------------- Main conversion

-- | Convert 'BibTeXDB' to 'BibDB'.
parseBib :: BibTeXDB -> BibDB
parseBib db = mapMaybe (parseBibEntry (getPreambles db)) db

-- Convert a single 'Reference' type BibTeX entry to a 'BibEntry'.
--
-- Fields are interpreted in the context of \@preamble.
-- Non-reference entry types are ignored.
parseBibEntry :: String -> BibTeXEntry -> Maybe BibEntry
parseBibEntry preamble (Reference rt rf) =
  let entryKey = maybe "" unwrapFieldValue (lookup "citekey" rf)
      -- prefix preamble to every field before TeX-ing it
      toTeX = parseTeXField preamble
      texFields = map (fmap toTeX . lowerCaseKeys) rf
      -- classify BibTeX field types, using predefined key lists like 'agentFields'
      (agents, (lists, others)) =
        partitionBy listFields <$> partitionBy agentFields texFields
      entryAgents = map (fmap parseAgents) agents
      entryLists = map (fmap parseList) lists
      entryFields = map (fmap tex2inlines) others
  in Just (BibEntry entryKey rt entryAgents entryLists entryFields)
parseBibEntry _ _ = Nothing


-------------------- Evaluate field content

-- Note: We cannot parse directly to 'Inline' elements, because some
-- fields (e.g. 'agentFields') need to be processed at \"brace level 0\",
-- i.e. before groups are flattened to inline elements.

-- Parse a BibTeX field value as TeX.
parseTeXField :: String -> FieldValue -> TeX
parseTeXField prefix = readTeX "bibfield"
  . (prefix++) . T.unpack . unwrapFieldValue

-- Collect BibTeX preambles.
getPreambles :: BibTeXDB -> String
getPreambles = T.unpack . T.concat . mapMaybe unwrapPreamble


-------------------- Parse literal lists

-- | Parse a literal lists.
--
-- Items are separated by \"and\" (at brace level 0).
parseList :: TeX -> [[Inline]]
parseList = filter (not . null)
  . map (stripInlines . tex2inlines)
  . splitTeXAtWord nameSep


-------------------- Parse name lists

-- | Parse a list of agents (e.g. person or company names),
-- typically from a BibTeX field value of type @name list@.
--
-- Field values are split into several components in three steps:
-- (1) into individual agent names separated by \"and\" (at brace level 0),
-- (2) into proto name parts separated by comma (\",\") (at brace level 0),
-- (3) into words separated by whitespace (at brace level 0).
parseAgents :: TeX -> [Agent]
parseAgents = mapMaybe parseAgent . splitTeXAtWord nameSep

-- | Parse a single agent name.
parseAgent :: TeX -> Maybe Agent
parseAgent xs =
  case map wordsTeX (splitTeXAtChar ',' xs) of
    [] -> Nothing
    (c1:[]) -> return $ from1 c1
    (c1:c2:[]) -> return $ from2 c1 c2
    (c1:c2:c3:cs) -> return $ (from2 c1 (c3 ++ concat cs))
                       { agentSuffix = (detex c2) }
  where
    mkAgent :: [TeX] -> [TeX] -> [TeX] -> [TeX] -> Agent
    mkAgent n1 n2 n3 n4 = Agent (detex n1) (detex n2) (detex n3) (detex n4)
    detex :: [TeX] -> [Inline]
    detex = stripInlines . tex2inlines . intercalate [White]
    -- If there are prefix names but no last name, use the final prefix name
    -- as the last name (even though it is in lower case).
    fillLast :: ([a], [a]) -> ([a], [a])
    fillLast (prefixnames, lastnames) =
      if null lastnames
      then (init prefixnames, [last prefixnames])
      else (prefixnames, lastnames)
    -- Extract agent name from a single field (no comma).
    from1 :: [TeX] -> Agent
    from1 ws =
      if any isLowerCase ws
      then let (firstnames, (prefixnames, lastnames))
                 = (fillLast . span isLowerCase) <$> break isLowerCase ws
           in mkAgent firstnames prefixnames lastnames []
      else let (firstnames, lastnames) = (init ws, [last ws])
           in mkAgent firstnames [] lastnames []
    -- Extract agent name from two fields (one comma).
    from2 :: [TeX] -> [TeX] -> Agent
    from2 ws firstnames =
      let (prefixnames, lastnames) = fillLast (span isLowerCase ws)
      in mkAgent firstnames prefixnames lastnames []


-------------------- TeX manipulation helpers (operating at brace level 0)

-- | Split TeX at a separating character.
--
-- The character must appear on brace level 0.
splitTeXAtChar :: Char -> TeX -> [TeX]
splitTeXAtChar _ [] = []
splitTeXAtChar c ((Plain xs@(_:_)):ts) =
  case break (==c) xs of
    -- case 1: no match
    (ys, []) ->
      case splitTeXAtChar c ts of
        [] -> [[Plain ys]]
        (u:us) -> (((Plain ys):u):us)
    -- case 2: match
    (ys, zs) ->
      let ys' = if null ys then [] else [Plain ys]
          zs' = case dropWhile (==c) zs of
                  [] -> ts
                  cs@(_:_) -> ((Plain cs):ts)
      in case splitTeXAtChar c zs' of
           [] -> [ys']
           us@(_:_) -> (ys':us)
splitTeXAtChar c (t:ts) =
  case splitTeXAtChar c ts of
    [] -> [[t]]
    (ys:zs) -> ((t:ys):zs)

-- | Split TeX at a separating word.
--
-- The word must be surrounded by white space on brace level 0.
splitTeXAtWord :: String -> TeX -> [TeX]
splitTeXAtWord sep = map unwordsTeX . (splitAnyAt [Plain sep]) . wordsTeX

-- Split list at a certain item.
splitAnyAt :: Eq a => a -> [a] -> [[a]]
splitAnyAt _ [] = []
splitAnyAt sep xs@(_:_) =
  let (ys, zs) = break (==sep) xs
  in (if null ys then id else (ys:))
     (splitAnyAt sep (dropWhile (==sep) zs))

-- | Break TeX into words.
--
-- A TeX word is a list of TeXAtoms that is
-- surrounded by whitespace on brace level 0.
wordsTeX :: TeX -> [TeX]
wordsTeX [] = []
wordsTeX xs@(_:_) =
  let (t, ts) = break isWhite (dropWhile isWhite xs)
  in t : wordsTeX (dropWhile isWhite ts)

-- | Combine TeX words by injecting top-level whitespace.
unwordsTeX :: [TeX] -> TeX
unwordsTeX = intercalate [White]

-- | Test whether a TeX word starts with a lower-case letter.
--
-- This is used by BibTeX to detect prefix name parts.
isLowerCase :: TeX -> Bool
isLowerCase [] = True
isLowerCase ((Plain (x:_)):_) = isLower x
isLowerCase ((Group _ _ xs):_) = isLowerCase (dropWhile isWhite xs)
isLowerCase _ = False


-------------------- Helpers for BibTeX fields

-- Helper for classifying BibTeX fields.
--
-- Partition a list of key-value pairs according to whether
-- the key is contained in a provided list of keys (e.g. 'agentFields').
partitionBy :: Eq a => [a] -> [(a, b)] -> ([(a, b)], [(a, b)])
partitionBy fs = partition ((`elem` fs) . fst)

-- Normalize BibTeX field name to lowercase.
lowerCaseKeys :: (Text, FieldValue) -> (Text, FieldValue)
lowerCaseKeys (k, v) = (T.toLower k, v)
