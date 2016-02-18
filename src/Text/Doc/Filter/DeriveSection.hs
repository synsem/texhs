----------------------------------------------------------------------
-- |
-- Module      :  Text.Doc.Filter.DeriveSection
-- Copyright   :  2015-2016 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  GHC
--
-- Document filter: Derive a structural section from a document;
-- for example, a table of contents or a bibliography.
----------------------------------------------------------------------

module Text.Doc.Filter.DeriveSection
 ( -- * Navigation generators
   NavList
 , NavListItem(..)
 , mkNavList
   -- * Section generators
 , addBibliography
 , addNotes
   -- * Document manipulation
 , ProtoSection
 , addAppendix
 ) where


import Control.Arrow (first)
import Control.Monad.Trans.State.Strict (State, runState, modify, gets)
import Data.List (nub, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Text.Doc.Section
import Text.Doc.Types


-------------------- document navigation: table of contents

-- | A navigation list.
type NavList = [NavListItem]

-- | A navigation list item with an internal anchor,
-- a title (section title) and a list of subsections.
--
-- (In other words: A 'Section' without level and body.)
data NavListItem = NavListItem InternalAnchor [Inline] NavList
  deriving (Eq, Show)

-- | Create a table of contents.
mkNavList :: [Section] -> NavList
mkNavList = map navItem

-- Create a navigation list item for a Section.
navItem :: Section -> NavListItem
navItem (Section _ anchor title _ subsecs) =
  NavListItem anchor title (map navItem subsecs)


-------------------- document manipulation

-- | A ProtoSection consists of a title, a body
-- and a list of proto-subsections.
data ProtoSection = ProtoSection [Inline] [Block] [ProtoSection]
  deriving (Eq, Show)

-- | Append a section to a document.
--
-- The section is added as a chapter in the 'Backmatter' region
-- of the document. This is intended for adding a bibliography
-- or a list of notes.
addAppendix :: SectionDoc -> ProtoSection -> SectionDoc
addAppendix (SectionDoc meta secs) appendix =
  let level = 2 -- assume chapter level
      makeAppendix = modify (registerRegion Backmatter) *>
                     appendSection level appendix
      (newSec, newMeta) = runState makeAppendix meta
  in SectionDoc newMeta (secs ++ [newSec])

-- Build a new (phantom) section for a document from a ProtoSection.
appendSection :: Level -> ProtoSection -> State Meta Section
appendSection level (ProtoSection title body protosubsecs) = do
  modify registerHeaderPhantom
  anchor <- gets getPhantomAnchor
  subsecs <- mapM (appendSection (level+1)) protosubsecs
  return $ Section level anchor title body subsecs


-------------------- bibliography

-- | Add bibliography as an appendix.
addBibliography :: SectionDoc -> SectionDoc
addBibliography doc = maybe doc (addAppendix doc) (mkBibliography doc)

-- | Derive a bibliography section.
--
-- Return 'Nothing' if the document contains no citations.
mkBibliography :: SectionDoc -> Maybe ProtoSection
mkBibliography (SectionDoc meta _) =
  let citeEntries = sort (M.elems (metaCiteDB meta))
  in if null citeEntries
     then Nothing
     else Just $
       ProtoSection [Str "Bibliography"] [BibList citeEntries] []


-------------------- notes

-- | Add notes section as an appendix.
addNotes :: SectionDoc -> SectionDoc
addNotes doc = maybe doc (addAppendix doc) (mkNotes doc)

-- | Derive a notes section.
--
-- Return 'Nothing' if the document contains no footnotes.
mkNotes :: SectionDoc -> Maybe ProtoSection
mkNotes (SectionDoc meta _) =
  let notes = metaNoteMap meta
      keys = M.keys notes             -- 'M.keys' returns sorted list
      chapters = (nub . map fst) keys -- only chapters with notes
      chapterNotes = map (mkChapterNotes notes) chapters
  in if null keys
     then Nothing
     else Just $
       ProtoSection [Str "Notes"] [] chapterNotes

-- Create a section for the notes of a specific chapter.
mkChapterNotes :: Map (Int, Int) [Block] -> Int -> ProtoSection
mkChapterNotes notes chapnum =
  let title = [Str "Chapter", Space, Str (show chapnum)]
      fndata = filter ((chapnum==) . fst . fst) (M.assocs notes)
      noteList = [AnchorList NoteList $ map (uncurry ListItem . first NoteAnchor) fndata]
  in ProtoSection title noteList []
