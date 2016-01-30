{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.Bib.Writer
-- Copyright   :  2015-2016 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  GHC
--
-- BibTeX formatter.
----------------------------------------------------------------------

module Text.Bib.Writer
  ( -- * Resolve
    resolveCitations
    -- * Query
  , getCiteAgents
    -- * Format
    -- ** Citation
  , fmtCiteAgents
  , fmtCiteYear
  , fmtExtraYear
  , CiteUnique
    -- ** Bibliography
  , fmtCiteFull
  , AgentFormat(..)
  , fmtAgent
  , fmtBibFieldAuthors
  ) where

import Control.Applicative
import Control.Monad (msum)
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Tuple (swap)

import Text.Bib.Types
import Text.Doc.Types


-------------------- Types

-- For some background, consult the biblatex documentation
-- on its counters @extrayear@, @uniquename@ and @uniquelist@.
-- | Information required to disambiguate author-year citations.
--
-- This stores whether an \"extrayear\" suffix is required to
-- distinguish multiple publications by the same author(s) in
-- the same year. A value of @Nothing@ represents that no suffix
-- is required, whereas @Just n@ indicates that it is the @n@-th
-- publication by the same author(s) in the same year.
type CiteUnique = Maybe Int


-------------------- Resolve

-- | Create a collection of formatted citations ('CiteEntry')
-- based on a set of citekeys (from 'CiteMap') and an underlying
-- bibliographic database ('BibDB').
resolveCitations :: BibDB -> CiteMap -> CiteDB
resolveCitations db citeMap =
  let uniqMap = mkCiteUniqueMap db citeMap
      resolve = resolveCitation db citeMap uniqMap
  in M.fromList $ mapMaybe resolve (M.keys citeMap)

-- Create a formatted citation ('CiteEntry') for a given citekey.
resolveCitation :: BibDB -> CiteMap -> Map CiteKey Int
                   -> CiteKey -> Maybe (CiteKey, CiteEntry)
resolveCitation bibdb citeMap uniqMap key =
  let uniqcite = M.lookup key uniqMap
      bibAnchor = BibAnchor <$> M.lookup key citeMap
  in (,) key <$> (mkCiteEntry uniqcite <$> bibAnchor <*> M.lookup key bibdb)

-- | Prepare author-year disambiguation information for every citekey.
--
-- Note: 'M.lookup' on this map returns a valid 'CiteUnique' value
-- (since unambiguous keys are not stored in this map).
mkCiteUniqueMap :: BibDB -> CiteMap -> Map CiteKey Int
mkCiteUniqueMap db citeMap =
  let keyEntryPairs :: [(CiteKey, BibEntry)]
      keyEntryPairs = mapMaybe -- drop unknown keys
        (\ key -> (,) key <$> M.lookup key db)
        (M.keys citeMap)
      inverseMap :: Map ([String], String) [CiteKey]
      inverseMap = foldr
        (uncurry (M.insertWith (++)) .
          fmap (:[]) . swap . fmap mkNameYearHash)
        M.empty
        keyEntryPairs
  in M.foldr (M.union . M.fromList . toCiteUnique db) M.empty inverseMap

-- Attach disambiguation label to a list of citekeys
-- with identical author-year key. Empty and singleton
-- lists need no disambiguation, so they are dropped.
--
-- Note: We are sorting keys using the Ord instance
-- for 'BibEntry'. This should match the method used
-- for sorting the bibliography, otherwise it could
-- happen that "2000a" is listed after "2000b"
-- (unless bibliography sorting considers extrayear
-- suffixes as part of the year).
toCiteUnique :: BibDB -> [CiteKey] -> [(CiteKey, Int)]
toCiteUnique _ [] = []
toCiteUnique _ [_] = []
toCiteUnique db ks@(_:_) = zip (sortBy (comparing (`M.lookup` db)) ks) [0..]


-------------------- Query

-- Extract citation information from a bib entry.
mkCiteEntry :: CiteUnique -> InternalAnchor -> BibEntry -> CiteEntry
mkCiteEntry uniqcite anchor entry = CiteEntry
  anchor
  (getCiteAgents entry)
  (fmtCiteYear uniqcite entry)
  (fmtCiteFull uniqcite entry)

-- Return a string representation of a name-year citation
-- for an entry (before disambiguation detection).
mkNameYearHash :: BibEntry -> ([String], String)
mkNameYearHash e =
  ( map (concatMap plain) (getCiteAgents e)
  , maybe "" (concatMap plain) (getBibLiteral "year" e))

-- | Retrieve a list of last names of authors or editors
-- for an author-year citation.
getCiteAgents :: BibEntry -> [[Inline]]
getCiteAgents entry =
  maybe [] (map agentLast)
    (getBibAgents "author" entry <|>
     getBibAgents "editor" entry)

-- | Construct year part of an author-year citation,
-- including an @extrayear@ suffix if required for
-- disambiguation in author-year citations.
fmtCiteYear :: CiteUnique -> BibEntry -> [Inline]
fmtCiteYear uniqcite = maybe [] appendExtrayear . getBibLiteral "year"
  where
    appendExtrayear = maybe id
      (flip (++) . (:[]) . Str . fmtExtraYear) uniqcite


-------------------- Citation Format

-- | Construct author part of an author-year citation
-- from a list of last names of authors.
fmtCiteAgents :: [[Inline]] -> [Inline]
fmtCiteAgents = fmtSepEndBy [Str ",", Space] [Space, Str "&", Space]

-- | Convert an integer to a textual @extrayear@ suffix that
-- can be appended to the year part in author-year citations.
-- For example, @0@ is converted to @\"a\"@, as in \"2000a\".
fmtExtraYear :: Int -> String
fmtExtraYear n = case n `divMod` 26 of
  (q,r) | q < 0 -> "" -- error: invalid negative value (ignore)
        | q == 0 -> [['a'..'z'] !! r]
        | otherwise -> fmtExtraYear (q-1) ++ [['a'..'z'] !! r]


-------------------- Bibliography Format

--------------------
-- To get started, we are hardcoding a specific citation style,
-- based on the \"unified\" style sheet for linguistics,
-- see: <http://www.linguisticsociety.org/resource/unified-style-sheet>.
-- Eventually this should be replaced by a configurable interface,
-- e.g. via a BibFormat module or direct citeproc (CSL) support.
--------------------

---------- main dispatch: by entry type

-- | Construct full bibliographic reference for an entry.
fmtCiteFull :: CiteUnique -> BibEntry -> [Inline]
fmtCiteFull u e@(BibEntry btype _) = case btype of
  "book" -> fmtBibBook u e
  "collection" -> fmtBibCollection u e
  "proceedings" -> fmtBibCollection u e
  "article" -> fmtBibArticle u e
  "incollection" -> fmtBibInCollection u e
  "inproceedings" -> fmtBibInCollection u e
  "thesis" -> fmtBibThesis Nothing u e
  "phdthesis" -> fmtBibThesis (Just [Str "dissertation"]) u e -- or "PhD thesis"
  "mastersthesis" -> fmtBibThesis (Just [Str "Master\x2019\&s thesis"]) u e
  _ -> fmtBibMisc u e


---------- entry types

-- Format a @book@ entry.
fmtBibBook :: CiteUnique -> BibEntry -> [Inline]
fmtBibBook u e =
  fmtSepBy
    (fmtBibFieldAuthors e)
    [Str ".", Space]
    (fmtBibBookAfterAgents u e)
  `fmtEndBy` [Str "."]

-- Format a @collection@ entry.
fmtBibCollection :: CiteUnique -> BibEntry -> [Inline]
fmtBibCollection u e =
  fmtSepBy
    (fmtBibFieldEditors e)
    [Str ".", Space]
    (fmtBibBookAfterAgents u e)
  `fmtEndBy` [Str "."]

-- Format a @thesis@ entry.
--
-- Takes an optional default thesis @type@ as first argument.
fmtBibThesis :: Maybe [Inline] -> CiteUnique -> BibEntry -> [Inline]
fmtBibThesis thesisType u e =
  fmtSepBy
    (fmtSepBy
      (fmtBibFieldAuthors e)
      [Str ".", Space]
      (fmtSepBy
        (fmtCiteYear u e)
        [Str ".", Space]
        (fmtEmph (fmtBibLiteral "title" e))))
    [Str ".", Space]
    (fmtSepBy
      (fmtBibFieldLocation e)
      [Str ":", Space]
      (fmtSepBy
        (fmtBibListAnyOf ["institution", "school"] e)
        [Space]
        (fromMaybe [Str "dissertation"]
          -- @type@ field overrides entry type
          (getBibLiteral "type" e <|> thesisType))))
  `fmtEndBy` [Str "."]

-- Format an @article@ entry.
fmtBibArticle :: CiteUnique -> BibEntry -> [Inline]
fmtBibArticle u e =
  fmtSepBy
    (fmtBibAYT u e)
    [Str ".", Space]
    (fmtSepBy
      (fmtSepBy
        (fmtEmph (fmtBibFieldJournaltitle e))
        [Space]
        (fmtSepBy
          (fmtBibLiteral "volume" e)
          []
          (fmtWrap [Str "("] (fmtBibFieldNumber e) [Str ")"])))
      [Str ".", Space]
      (fmtBibLiteral "pages" e))
  `fmtEndBy` [Str "."]

-- Format an @incollection@ entry.
fmtBibInCollection :: CiteUnique -> BibEntry -> [Inline]
fmtBibInCollection u e =
  fmtSepBy
    (fmtBibAYT u e)
    [Str ".", Space]
    (fmtWrap
      [Str "In", Space]
      (fmtSepBy
        (fmtSepBy
          (fmtBibFieldEditorsInner e)
          [Str ",", Space]
          (fmtSepBy
            (fmtEmph $ fmtBibLiteral "booktitle" e)
            [Str ",", Space]
            (fmtBibLiteral "pages" e)))
        [Str ".", Space]
        (fmtSepBy
          (fmtBibFieldLocation e)
          [Str ":", Space]
          (fmtBibList "publisher" e)))
      [])
  `fmtEndBy` [Str "."]

-- Format a @misc@ entry. Also used as fallback entry type.
fmtBibMisc :: CiteUnique -> BibEntry -> [Inline]
fmtBibMisc u e =
  let ag = if M.member "author" (bibFields e)
           then fmtBibFieldAuthors e
           else fmtBibFieldEditors e
  in fmtSepBy ag [Str ".", Space]
       (fmtSepBy (fmtCiteYear u e) [Str ".", Space]
         (fmtSepBy (fmtBibLiteral "title" e) [Str ".", Space]
           (fmtBibLiteral "howpublished" e)))
     `fmtEndBy` [Str "."]


---------- shared multi-field blocks

-- Common format for @book@ and @collection@ after agent part.
fmtBibBookAfterAgents :: CiteUnique -> BibEntry -> [Inline]
fmtBibBookAfterAgents u e =
  fmtSepBy
    (fmtCiteYear u e)
    [Str ".", Space]
    (fmtSepBy
      (fmtEmph (fmtBibLiteral "title" e))
      [Str ".", Space]
      (fmtSepBy
        (fmtBibFieldLocation e)
        [Str ":", Space]
        (fmtBibList "publisher" e)))

-- Format \"Author. Year. Title\", common prefix of many entry types
-- (e.g. @article@, @incollection@). The @title@ part is not emphasized.
fmtBibAYT :: CiteUnique -> BibEntry -> [Inline]
fmtBibAYT u e =
  fmtSepBy
    (fmtBibFieldAuthors e)
    [Str ".", Space]
    (fmtSepBy
      (fmtCiteYear u e)
      [Str ".", Space]
      (fmtBibLiteral "title" e))


---------- entry fields

-- | Retrieve @author@ field and format as primary (outer) agents.
fmtBibFieldAuthors :: BibEntry -> [Inline]
fmtBibFieldAuthors = maybe [] fmtAgentsOuter . getBibAgents "author"

-- Retrieve @editor@ field and format as primary (outer) agents.
fmtBibFieldEditors :: BibEntry -> [Inline]
fmtBibFieldEditors =
  maybe [] (\ xs -> fmtAgentsOuter xs `fmtEndBy` editorSuffix xs) .
  getBibAgents "editor"

-- Retrieve @editor@ field and format as secondary (inner) agents.
fmtBibFieldEditorsInner :: BibEntry -> [Inline]
fmtBibFieldEditorsInner =
  maybe [] (\xs -> fmtAgentsInner xs `fmtEndBy` editorSuffix xs) .
  getBibAgents "editor"

-- Retrieve @location@ (or @address@) list field.
fmtBibFieldLocation :: BibEntry -> [Inline]
fmtBibFieldLocation = fmtBibListAnyOf
  [ "location"
  , "address" ] -- legacy bibtex

-- Retrieve @journaltitle@ (or @journal@) field.
fmtBibFieldJournaltitle :: BibEntry -> [Inline]
fmtBibFieldJournaltitle = fmtBibLiteralAnyOf
  [ "journaltitle"
  , "journal" ] -- legacy bibtex

-- Retrieve @number@ or @issue@ field.
fmtBibFieldNumber :: BibEntry -> [Inline]
fmtBibFieldNumber = fmtBibLiteralAnyOf
  [ "number"
  , "issue" ]


---------- agents

-- | Format for arranging the name parts of an agent.
data AgentFormat
  = LastFirst   -- \"Last, First von, Jr\", used for the first primary (outer) agent.
  | FirstLast   -- \"First von Last, Jr\", used for secondary (inner) agents.
  deriving (Eq, Show)

-- | Format an agent name according to some format.
fmtAgent :: AgentFormat -> Agent -> [Inline]
fmtAgent LastFirst (Agent f p l s) =
  fmtSepBy l [Str ",", Space] (fmtSepBy (fmtSepBy f [Space] p) [Str ",", Space] s)
fmtAgent FirstLast (Agent f p l s) =
  fmtSepBy (fmtSepBy f [Space] (fmtSepBy p [Space] l)) [Str ",", Space] s

-- Format a list of primary (outer) agents, typically at the beginning
-- of a bibliographic entry. The first agent is formatted 'LastFirst',
-- the others are formatted 'FirstLast'.
fmtAgentsOuter :: [Agent] -> [Inline]
fmtAgentsOuter [] = []
fmtAgentsOuter (x:xs) =
  fmtSepEndBy [Str ",", Space] [Space, Str "&", Space]
    (fmtAgent LastFirst x : map (fmtAgent FirstLast) xs)

-- Format a list of secondary (inner) agents, typically within a
-- bibliographic entry. All agents are formatted 'FirstLast'.
fmtAgentsInner :: [Agent] -> [Inline]
fmtAgentsInner =
  fmtSepEndBy [Str ",", Space] [Space, Str "&", Space] .
  map (fmtAgent FirstLast)

-- Determine editor suffix, i.e. \"(ed.)\" or \"(eds.)\".
editorSuffix :: [Agent] -> [Inline]
editorSuffix [_] = [Space, Str "(ed.)"]
editorSuffix _ = [Space, Str "(eds.)"]


---------- helpers: extract values from entry fields

-- Try to retrieve an unwrapped literal field from a BibEntry.
-- Return the empty list in case of failure.
fmtBibLiteral :: BibFieldName -> BibEntry -> [Inline]
fmtBibLiteral name = fromMaybe [] . getBibLiteral name

-- Try to retrieve an unwrapped literal field from a BibEntry.
-- Return the value for the first field name that exists,
-- or the empty list in case of failure.
fmtBibLiteralAnyOf :: [BibFieldName] -> BibEntry -> [Inline]
fmtBibLiteralAnyOf names entry = fromMaybe [] $
  msum (map (`getBibLiteral` entry) names)

-- Try to retrieve an unwrapped literal list from a BibEntry.
-- Return the empty list in case of failure.
fmtBibList :: BibFieldName -> BibEntry -> [Inline]
fmtBibList name = maybe []
  (fmtSepEndBy [Str ",", Space] [Space, Str "&", Space]) .
  getBibList name

-- Try to retrieve an unwrapped literal list from a BibEntry.
-- Return the value for the first field name that exists,
-- or the empty list in case of failure.
fmtBibListAnyOf :: [BibFieldName] -> BibEntry -> [Inline]
fmtBibListAnyOf names entry = fromMaybe [] $
  fmtSepEndBy [Str ",", Space] [Space, Str "&", Space] <$>
  msum (map (`getBibList` entry) names)
