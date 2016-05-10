{-# LANGUAGE CPP #-}
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
  , fmtMultiCite
  , fmtSingleCite
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

#if MIN_VERSION_base(4,8,0)
#else
import Data.Monoid (mempty)
#endif
import Control.Applicative
import Control.Monad (msum)
import Data.Function (on)
import Data.List (groupBy, intercalate, sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (comparing)
import qualified Data.Text as T
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

-- | Construct a multicite inline citation.
fmtMultiCite :: CiteDB -> MultiCite -> [Inline]
fmtMultiCite db multiCite@(MultiCite citemode _ _ _) =
  let wrapper = case citemode of
        CiteParen -> flip (fmtWrap [Str "("]) [Str ")"]
        _ -> id
  in wrapper $ fmtMultiCiteInner db multiCite

-- Construct the inner part of a multicite inline citation.
fmtMultiCiteInner :: CiteDB -> MultiCite -> [Inline]
fmtMultiCiteInner db (MultiCite citemode prenote postnote cs) =
  fmtSepBy
    (fmtSepBy
      prenote
      [Space]
      ((if citemode == CiteText
        then fmtSepEndBy [Str ",", Space] [Str ",", Space, Str "and", Space]
        else intercalate [Str ";", Space])
       (map (fmtSingleCite db citemode) cs)))
    [Str ",", Space]
    postnote

-- | Construct a singlecite inline citation.
fmtSingleCite :: CiteDB -> CiteMode -> SingleCite -> [Inline]
fmtSingleCite db citemode (SingleCite prenote postnote keys) =
  let entries = groupCitesByAuthor (mapMaybe (`M.lookup` db) keys)
      -- prenote (postnote) is added to first (last) citekey
      fillers = replicate (max 0 (length entries - 1)) []
      prenotes = prenote : fillers
      postnotes = fillers ++ [postnote]
      sepKeys = [Str ";", Space]
  in intercalate sepKeys $
     zipWith3 (fmtCiteGroup citemode) prenotes postnotes entries

-- Group a list of citations (within a SingleCite) by author.
--
-- This is used to compress adjacent citations of bibentries
-- by the same author or editor. For example, instead of
-- \"Doe 1999; Doe 2000\", we want \"Doe 1999, 2000\".
groupCitesByAuthor :: [CiteEntry] -> [[CiteEntry]]
groupCitesByAuthor = groupBy ((==) `on` citeAgents)

-- Format a list of cite entries (by the same author).
--
-- Assumption: All cite entries share the same agents (authors or editors).
fmtCiteGroup :: CiteMode -> [Inline] -> [Inline] -> [CiteEntry] -> [Inline]
fmtCiteGroup _ _ _ [] = mempty
fmtCiteGroup CiteBare pre post entries@(e:_) =
  fmtSepBy
    (fmtSepBy
      pre
      [Space]
      (fmtSepBy
        (mkInternalLink
          (fmtCiteAgents (citeAgents e))
          (citeAnchor e)
          (T.pack (concatMap plain (citeFull e)))
          "citation")
        [Space]
        (intercalate
          [Str ",", Space]
          (map (\ i -> mkInternalLink
             (citeYear i)
             (citeAnchor i)
             (T.pack (concatMap plain (citeFull i)))
             "citation")
           entries))))
    [Str ",", Space]
    post
fmtCiteGroup CiteParen pre post entries =
  fmtCiteGroup CiteBare pre post entries
fmtCiteGroup CiteText pre post entries@(e:_) =
  fmtSepBy
    (mkInternalLink
      (fmtCiteAgents (citeAgents e))
      (citeAnchor e)
      (T.pack (concatMap plain (citeFull e)))
      "citation")
    [Space]
    (fmtWrap
      [Str "("]
      (fmtSepBy
        (fmtSepBy
          pre
          [Space]
          (intercalate
            [Str ",", Space]
            (map (\ i -> mkInternalLink
                (citeYear i)
                (citeAnchor i)
                (T.pack (concatMap plain (citeFull i)))
                "citation")
              entries)))
        [Str ",", Space]
        post)
      [Str ")"])
fmtCiteGroup CiteAuthor pre post (e:_) =
  -- like CiteBare but without the years part
  fmtSepBy
    (fmtSepBy
      pre
      [Space]
      (mkInternalLink
        (fmtCiteAgents (citeAgents e))
        (citeAnchor e)
        (T.pack (concatMap plain (citeFull e)))
        "citation"))
    [Str ",", Space]
    post
fmtCiteGroup CiteYear pre post entries@(_:_) =
  -- like CiteBare but without the author part
  fmtSepBy
    (fmtSepBy
      pre
      [Space]
      (intercalate
        [Str ",", Space]
        (map (\ i -> mkInternalLink
          (citeYear i)
          (citeAnchor i)
          (T.pack (concatMap plain (citeFull i)))
          "citation")
         entries)))
    [Str ",", Space]
    post


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
  fmtEndByDot $ fmtSepByDot
    (fmtBibFieldAuthors e)
    (fmtBibBookAfterAgents u e)

-- Format a @collection@ entry.
fmtBibCollection :: CiteUnique -> BibEntry -> [Inline]
fmtBibCollection u e =
  fmtEndByDot $ fmtSepByDot
    (fmtBibFieldEditors e)
    (fmtBibBookAfterAgents u e)

-- Format a @thesis@ entry.
--
-- Takes an optional default thesis @type@ as first argument.
fmtBibThesis :: Maybe [Inline] -> CiteUnique -> BibEntry -> [Inline]
fmtBibThesis thesisType u e =
  fmtEndByDot $ fmtSepByDot
    (fmtSepByDot
      (fmtBibFieldAuthors e)
      (fmtSepByDot
        (fmtCiteYear u e)
        (fmtEmph (fmtBibLiteral "title" e))))
    (fmtSepBy
      (fmtBibFieldLocation e)
      [Str ":", Space]
      (fmtSepBy
        (fmtBibList "institution" e)
        [Space]
        (fromMaybe [Str "dissertation"]
          -- @type@ field overrides entry type
          (getBibLiteral "type" e <|> thesisType))))

-- Format an @article@ entry.
fmtBibArticle :: CiteUnique -> BibEntry -> [Inline]
fmtBibArticle u e =
  fmtEndByDot $ fmtSepByDot
    (fmtBibAYT u e)
    (fmtSepByDot
      (fmtSepBy
        (fmtEmph (fmtBibFieldJournaltitle e))
        [Space]
        (fmtSepBy
          (fmtBibLiteral "volume" e)
          []
          (fmtWrap [Str "("] (fmtBibFieldNumber e) [Str ")"])))
      (fmtBibLiteral "pages" e))

-- Format an @incollection@ entry.
fmtBibInCollection :: CiteUnique -> BibEntry -> [Inline]
fmtBibInCollection u e =
  fmtEndByDot $ fmtSepByDot
    (fmtBibAYT u e)
    (fmtWrap
      [Str "In", Space]
      (fmtSepByDot
        (fmtSepBy
          (fmtBibFieldEditorsInner e)
          [Str ",", Space]
          (fmtSepBy
            (fmtEmph $ fmtBibLiteral "booktitle" e)
            [Str ",", Space]
            (fmtBibLiteral "pages" e)))
        (fmtSepBy
          (fmtBibFieldLocation e)
          [Str ":", Space]
          (fmtBibList "publisher" e)))
      [])

-- Format a @misc@ entry. Also used as fallback entry type.
fmtBibMisc :: CiteUnique -> BibEntry -> [Inline]
fmtBibMisc u e =
  let ag = if M.member "author" (bibFields e)
           then fmtBibFieldAuthors e
           else fmtBibFieldEditors e
  in fmtEndByDot $ fmtSepByDot ag
       (fmtSepByDot (fmtCiteYear u e)
         (fmtSepByDot (fmtBibLiteral "title" e)
           (fmtBibLiteral "howpublished" e)))


---------- shared multi-field blocks

-- Common format for @book@ and @collection@ after agent part.
fmtBibBookAfterAgents :: CiteUnique -> BibEntry -> [Inline]
fmtBibBookAfterAgents u e =
  fmtSepByDot
    (fmtCiteYear u e)
    (fmtSepByDot
      (fmtEmph (fmtBibLiteral "title" e))
      (fmtSepBy
        (fmtBibFieldLocation e)
        [Str ":", Space]
        (fmtBibList "publisher" e)))

-- Format \"Author. Year. Title\", common prefix of many entry types
-- (e.g. @article@, @incollection@). The @title@ part is not emphasized.
fmtBibAYT :: CiteUnique -> BibEntry -> [Inline]
fmtBibAYT u e =
  fmtSepByDot
    (fmtBibFieldAuthors e)
    (fmtSepByDot
      (fmtCiteYear u e)
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

-- Retrieve @location@ list field.
fmtBibFieldLocation :: BibEntry -> [Inline]
fmtBibFieldLocation = fmtBibList "location"

-- Retrieve @journaltitle@ field.
fmtBibFieldJournaltitle :: BibEntry -> [Inline]
fmtBibFieldJournaltitle = fmtBibLiteral "journaltitle"

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
