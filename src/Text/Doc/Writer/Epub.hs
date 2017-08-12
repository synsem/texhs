{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.Doc.Writer.Epub
-- Copyright   :  2015-2017 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
--
-- Maintainer  :  mschenner.dev@gmail.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- EPUB writer: Convert Doc to EPUB format.
----------------------------------------------------------------------

module Text.Doc.Writer.Epub
 ( -- * Doc to EPUB Conversion
   doc2epub
   -- * Types
 , Epub
 , EpubContainer(..)
 , EpubData(..)
 , EpubMedia(..)
 , EpubMeta(..)
 , File
 , TextFile
 ) where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative
import Data.Monoid (mempty)
#endif
import Codec.Archive.Zip ( Archive, Entry, toEntry, addEntryToArchive
                         , emptyArchive, fromArchive)
import Control.Monad (msum)
import Data.Bits (clearBit, setBit)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlpha, isAlphaNum, isAscii)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.Format ( defaultTimeLocale, iso8601DateFormat
                        , formatTime, parseTimeM)
import Data.Word (Word64)
import System.FilePath (takeExtension)
import System.IO.Error (tryIOError)
import System.Random (randomIO)
import Text.Blaze (Markup, (!), string, text, stringValue, textValue)
import Text.Blaze.Renderer.Text (renderMarkup)
import Text.Printf (printf)

import Text.Doc.Types
import Text.Doc.Section
import Text.Doc.Filter.DeriveSection
import Text.Doc.Filter.MultiFile
import Text.Doc.Writer.Core
import Text.Doc.Writer.Html


-------------------- Conversion

-- | Convert a document to an EPUB file.
doc2epub :: Doc -> IO ByteString
doc2epub doc = do
  let mdoc = prepDoc doc
  epubMeta <- mkEpubMeta mdoc
  (ndoc, epubMedia) <- extractMedia mdoc
  timestamp <- getEpochTime
  return $ writeEpub timestamp (mkEpub ndoc epubMeta epubMedia)

-- Convert a raw document to a multi-file document
-- with auto-generated notes and bibliography sections.
prepDoc :: Doc -> MultiFileDoc
prepDoc = toMultiFileDoc 3 . addBibliography . addNotes .
  doc2secdoc . setHtmlVersion XHTML1

-- Create a structured representation of an EPUB document.
mkEpub :: MultiFileDoc -> EpubMeta -> EpubMedia -> Epub
mkEpub mdoc epubMeta epubMedia =
  let epubData = mkEpubData mdoc epubMeta
      epubCont = mkEpubContainer epubMeta epubData epubMedia
  in (epubCont, epubData, epubMedia)

-- Serialize an EPUB document into its binary form.
writeEpub :: EpochTime -> Epub -> ByteString
writeEpub timestamp = fromArchive . mkEpubArchive timestamp

-- Create a ZIP Archive from a structured EPUB representation.
mkEpubArchive :: EpochTime -> Epub -> Archive
mkEpubArchive timestamp epub =
  let mkEntry :: File -> Entry
      mkEntry (path, content) = toEntry path timestamp content
      entries = map mkEntry (getEpubFiles epub)
  in foldr addEntryToArchive emptyArchive entries

-- Generate EPUB meta infos.
--
-- Requires IO for generating a random UUID and
-- for retrieving the current date.
--
-- Step 1 of 3 in the EPUB creation pipeline.
mkEpubMeta :: MultiFileDoc -> IO EpubMeta
mkEpubMeta (MultiFileDoc meta nav _) = do
  uuid <- newRandomUUID
  date <- getFormattedDate (concatMap plain (metaDate meta))
  let fromInlines = T.pack . concatMap plain
  return EpubMeta
    { epubMetaTitle = fromInlines (metaTitle meta)
    , epubMetaSubTitle = fromInlines (metaSubTitle meta)
    , epubMetaAuthors = map fromInlines (metaAuthors meta)
    , epubMetaDate = T.pack date
    , epubMetaUUID = T.pack uuid
    , epubMetaNavMap = nav
    , epubMetaAnchorDB = metaAnchorFileMap meta
    }

-- Generate EPUB data files (content documents).
--
-- Step 2 of 3 in the EPUB creation pipeline.
mkEpubData :: MultiFileDoc -> EpubMeta -> EpubData
mkEpubData doc epubMeta = EpubData
  { epubTitlePage = ("titlepage.xhtml", htmlTitlePage doc)
  , epubNavPage = ("toc.xhtml", htmlNavPage doc)
  , epubBody = M.assocs (mdoc2epubPages doc)
  , epubNCX = ("toc.ncx", renderMarkup (mkNCX epubMeta))
  }

-- Generate EPUB container files.
--
-- Step 3 of 3 in the EPUB creation pipeline.
mkEpubContainer :: EpubMeta -> EpubData -> EpubMedia -> EpubContainer
mkEpubContainer epubMeta epubData epubMedia =
  let opfPath = "content.opf"
  in EpubContainer
       { epubOPF = (opfPath, renderMarkup (mkOPF epubMeta epubData epubMedia))
       , epubContainerXml = ( "META-INF/container.xml"
                            , renderMarkup (mkContainerXml opfPath))
       , epubMimetype = ("mimetype", "application/epub+zip")
       }


-------------------- Types

---------- Files

-- | A binary file represented by its file path and content.
type File = (FilePath, ByteString)

-- | A text file represented by its file path and content.
type TextFile = (FilePath, TL.Text)

-- Convert a text file to a UTF-8 encoded binary file.
toFile :: TextFile -> File
toFile = fmap TL.encodeUtf8


---------- EPUB documents

-- | A structured representation of a complete EPUB document.
type Epub = (EpubContainer, EpubData, EpubMedia)

-- | EPUB top-level packaging files.
data EpubContainer = EpubContainer
  { -- | The package document (OPF).
    epubOPF :: TextFile
    -- | The OCF container file (@META-INF/container.xml@).
  , epubContainerXml :: TextFile
    -- | The @mimetype@ file.
  , epubMimetype :: TextFile
  } deriving (Eq, Show)

-- | EPUB textual data files.
--
-- This includes all content documents
-- as well as title and navigation pages.
data EpubData = EpubData
  { epubTitlePage :: TextFile
  , epubNavPage :: TextFile
  , epubBody :: [TextFile]
  , epubNCX :: TextFile
  } deriving (Eq, Show)

-- | EPUB media files.
--
-- This includes all referenced image files.
data EpubMedia = EpubMedia
  { epubMediaCover :: Maybe File
  , epubMediaFigures :: [File]
  } deriving (Eq, Show)

-- | EPUB document meta information.
data EpubMeta = EpubMeta
  { epubMetaTitle :: Text
  , epubMetaSubTitle :: Text
  , epubMetaAuthors :: [Text]
  , epubMetaDate :: Text
  , epubMetaUUID :: Text
  , epubMetaNavMap :: NavList
  , epubMetaAnchorDB :: AnchorFileMap
  } deriving (Eq, Show)


---------- Extract files

-- Create a list of all files contained in an EPUB document.
--
-- Note: The order of these files corresponds to their order in the
-- ZIP Archive, so the @mimetype@ file must come first.
getEpubFiles :: Epub -> [File]
getEpubFiles (ec, ed, em) =
  map toFile
    ( getEpubContainerFiles ec ++
      getEpubDataFiles ed) ++
  getEpubMediaFiles em

-- Extract all EPUB textual data files.
--
-- Note: The order of these files corresponds to their order in the
-- ZIP Archive, so the @mimetype@ file must come first.
getEpubContainerFiles :: EpubContainer -> [TextFile]
getEpubContainerFiles (EpubContainer opf cont mime) =
  [mime,cont,opf]

-- Extract all EPUB textual data files.
getEpubDataFiles :: EpubData -> [TextFile]
getEpubDataFiles (EpubData title nav body ncx) =
  ncx : title : nav : body

-- Extract all EPUB media files.
getEpubMediaFiles :: EpubMedia -> [File]
getEpubMediaFiles (EpubMedia cover figures) =
  maybe id (:) cover figures


---------- Extract filepaths

-- Extract all filepaths of EPUB data documents
-- and included media files.
--
-- Note: This does not include the filepaths of
-- EPUB container files (see @EpubContainer@).
getEpubPaths :: EpubData -> EpubMedia -> [FilePath]
getEpubPaths epubData epubMedia =
  getEpubDataPaths epubData ++
  getEpubMediaPaths epubMedia

-- Extract all filepaths of EPUB data documents.
--
-- Note: This does not include the filepaths of
-- referenced media files (see @EpubMedia@).
getEpubDataPaths :: EpubData -> [FilePath]
getEpubDataPaths = map fst . getEpubDataFiles

-- Extract all filepaths of EPUB media files.
getEpubMediaPaths :: EpubMedia -> [FilePath]
getEpubMediaPaths = map fst . getEpubMediaFiles


-------------------- IO functions

---------- IO: media files

-- Extract and rename media files.
extractMedia :: MultiFileDoc -> IO (MultiFileDoc, EpubMedia)
extractMedia (MultiFileDoc meta nav body) = do
  let coverPath = metaCover meta
      mediaMap = metaMediaMap meta
      mediaDblPathMap = M.mapWithKey renameMediaFile mediaMap
      newMediaMap = M.map snd mediaDblPathMap
      newMeta = meta { metaMediaMap = newMediaMap }
  figures <- mkMediaFileMap (M.elems mediaDblPathMap)
  cover <- readCoverImage coverPath
  return (MultiFileDoc newMeta nav body, EpubMedia cover figures)

-- Try to read all referenced media files
-- and collect successful results.
mkMediaFileMap :: [(Location, Location)] -> IO [File]
mkMediaFileMap paths = catMaybes <$> mapM readMediaFile paths

-- Try to read a single media file.
readMediaFile :: (Location, Location) -> IO (Maybe File)
readMediaFile (loc, newloc) =
  either (const Nothing) (\bs -> Just (T.unpack newloc, bs)) <$>
    tryIOError (BL.readFile (T.unpack loc))

-- Try to read the cover image.
readCoverImage :: Maybe FilePath -> IO (Maybe File)
readCoverImage Nothing = return Nothing
readCoverImage (Just coverPath) =
  let ext = takeExtension coverPath
  in readMediaFile (T.pack coverPath, T.pack (printf "figures/cover%s" ext))

-- Generate a name of a media file based on its
-- MediaID and its original file extension.
-- Return the original and the new filepath as a pair.
renameMediaFile :: MediaID -> Location -> (Location, Location)
renameMediaFile i loc =
  let ext = takeExtension (T.unpack loc)
  in (loc, T.pack (printf "figures/image-%03d%s" i ext))


---------- IO: date and time

-- | Epoch time is an integer representation of POSIX time.
--
-- This is the time format required for Entries in a ZIP Archive.
type EpochTime = Integer

-- | Get current timestamp as an EpochTime value.
getEpochTime :: IO EpochTime
getEpochTime = floor <$> getPOSIXTime

-- | Create date in @YYYY[-MM[-DD]]@ format.
--
-- Try to parse the input string as @YYYY[-MM[-DD]]@ and return it.
-- If that fails, format the current day as @YYYY-MM-DD@ (ISO-8601).
--
-- Invalid values for month and day will be sanitized to ensure correct
-- dates. For example, @\"2016-80\"@ is changed to @\"2016-12\"@.
getFormattedDate :: String -> IO String
getFormattedDate userDate =
  let parseUserDateAs :: String -> Maybe UTCTime
      parseUserDateAs = flip (parseTimeM True defaultTimeLocale) userDate
      parsedUserDate = msum $ map parseUserDateAs ["%0Y", "%0Y-%m", "%0Y-%m-%d"]
      formatDate = formatTime defaultTimeLocale (iso8601DateFormat Nothing)
  in case parsedUserDate of
       Nothing -> formatDate <$> getCurrentTime
       Just date -> return $ take (length userDate) (formatDate date)


---------- IO: random

-- | Generate a random (version 4) UUID.
newRandomUUID :: IO String
newRandomUUID = do
  let setVersion = (`clearBit` 15) . (`setBit` 14) .
                   (`clearBit` 13) . (`clearBit` 12)
      setVariant = (`setBit` 63) . (`clearBit` 62)
  upper <- setVersion <$> randomIO :: IO Word64
  lower <- setVariant <$> randomIO :: IO Word64
  let uuid = concatMap (printf "%016x") [upper, lower] :: String
  return $ "urn:uuid:" ++ intercalate "-" (splitAtN [8,4,4,4] uuid)

-- Split a list at the specified relative prefix lengths.
splitAtN :: [Int] -> [a] -> [[a]]
splitAtN [] xs = [xs]
splitAtN (n:ns) xs =
  let (ys,zs) = splitAt n xs
  in ys : splitAtN ns zs


-------------------- Generate EPUB packaging files

---------- Generate OCF Container file

-- | The OCF Container file (@META-INF/container.xml@).
mkContainerXml :: FilePath -> Markup
mkContainerXml opfPath = withXmlDeclaration $
  el "container" !
    attr "version" "1.0" !
    attr "xmlns" "urn:oasis:names:tc:opendocument:xmlns:container" $
    el "rootfiles" $
      leaf "rootfile" !
      attr "full-path" (stringValue opfPath) !
      attr "media-type" "application/oebps-package+xml"


---------- Generate package document (OPF)

-- | The package document (OPF).
mkOPF :: EpubMeta -> EpubData -> EpubMedia -> Markup
mkOPF epubMeta epubData epubMedia = withXmlDeclaration $ do
  let idref = stringValue . mkNCName . fst
  el "package" !
    attr "version" "2.0" !
    attr "xmlns" "http://www.idpf.org/2007/opf" !
    attr "unique-identifier" "BookId" $ do
    el "metadata" !
      attr "xmlns:dc" "http://purl.org/dc/elements/1.1/" !
      attr "xmlns:opf" "http://www.idpf.org/2007/opf" $ do
      el "dc:title" (text (epubMetaTitle epubMeta))
      el "dc:language" "en"
      el "dc:identifier" !
        attr "id" "BookId" !
        attr "opf:scheme" "UUID" $
        text (epubMetaUUID epubMeta)
      mapM_ ((el "dc:creator" ! attr "opf:role" "aut") . text)
        (epubMetaAuthors epubMeta)
      el "dc:date" (text (epubMetaDate epubMeta))
      case epubMediaCover epubMedia of
        Nothing -> mempty
        Just cover -> leaf "meta" !
          attr "name" "cover" !
          attr "content" (idref cover)
    el "manifest" $
      mapM_ (opfItem2xml . mkOpfItem)
        (getEpubPaths epubData epubMedia)
    el "spine" ! attr "toc" (idref (epubNCX epubData)) $ do
      leaf "itemref" !
        attr "idref" (idref (epubTitlePage epubData)) !
        attr "linear" "no"
      leaf "itemref" !
        attr "idref" (idref (epubNavPage epubData)) !
        attr "linear" "no"
      mapM_ (\i -> leaf "itemref" ! attr "idref" (idref i))
        (epubBody epubData)

-- Representation of an @\<item\>@ entry in the OPF manifest
-- in the form: @(id-value, href-value, media-type)@.
type OpfItem = (String, FilePath, String)

-- Create an OPF manifest item from a filepath.
mkOpfItem :: FilePath -> OpfItem
mkOpfItem filepath =
  let idValue = mkNCName filepath
      mediaType = mimetypeOf filepath
  in (idValue, filepath, mediaType)

-- Create XML fragment for an OPF manifest item.
opfItem2xml :: OpfItem -> Markup
opfItem2xml (idValue, filepath, mediaType) =
  leaf "item" !
    attr "id" (stringValue idValue) !
    attr "href" (stringValue filepath) !
    attr "media-type" (stringValue mediaType)

-- Convert a filename to a string that is somewhat better suited as
-- a value of an @xml:id@ attribute. Does not ensure uniqueness.
mkNCName :: FilePath -> String
mkNCName [] = "unknown"
mkNCName xs@(x:_) = alphaPrefix (map sanitize xs)
  where
    -- Prefix underscore if first char is not a letter.
    alphaPrefix = if isAlpha x then id else ('_':)
    -- Keep only alphanumeric (or whitelisted) ascii chars,
    -- replace all other chars by dashes.
    whitelist = "_-" :: String
    sanitize c
      | isAscii c && (isAlphaNum c || c `elem` whitelist) = c
      | otherwise = '-'

-- Guess the MIME Media Type of a file from its name.
mimetypeOf :: FilePath -> String
mimetypeOf filepath =
  fromMaybe "application/xhtml+xml" $ -- defaulting to xhtml
    M.lookup (takeExtension filepath) mimetypeMap

-- Simple mapping from filename extensions
-- to corresponding MIME Media Types.
--
-- Limited to selected EPUB content types.
mimetypeMap :: Map String String
mimetypeMap = M.fromList
  [ -- content documents
    (".xhtml", "application/xhtml+xml")
  , (".html", "text/html")
  , (".xml", "application/xml")
  , (".css", "text/css")
    -- image formats
  , (".svg", "image/svg+xml")
  , (".png", "image/png")
  , (".jpg", "image/jpeg")
  , (".jpeg", "image/jpeg")
  , (".gif", "image/gif")
    -- epub xml formats
  , (".ncx", "application/x-dtbncx+xml")
  ]


---------- Generate NCX file

-- | The \"Navigation Center eXtended\" (NCX) file.
mkNCX :: EpubMeta -> Markup
mkNCX epubMeta = withXmlDeclaration $
  el "ncx" !
    attr "version" "2005-1" !
    attr "xmlns" "http://www.daisy.org/z3986/2005/ncx/" $ do
    el "head" $ do
      leaf "meta" ! attr "name" "dtb:uid" !
        attr "content" (textValue (epubMetaUUID epubMeta))
      leaf "meta" ! attr "name" "dtb:depth" !
        attr "content" (stringValue (show (navDepth (epubMetaNavMap epubMeta))))
      leaf "meta" ! attr "name" "dtb:totalPageCount" ! attr "content" "0"
      leaf "meta" ! attr "name" "dtb:maxPageNumber" ! attr "content" "0"
    el "docTitle" . el "text" . text $ epubMetaTitle epubMeta
    mapM_ (el "docAuthor" . el "text" . text) (epubMetaAuthors epubMeta)
    mkNavMap (epubMetaAnchorDB epubMeta) (epubMetaNavMap epubMeta)

-- Calculate maximum depth of a NavList
-- (for the NCX @\<meta name=\"dtb:depth\"\>@  element).
navDepth :: NavList -> Int
navDepth xs = maximum (0: map itemDepth xs)
  where itemDepth (NavListItem _ _ subitems) = 1 + navDepth subitems

-- Generate a @\<navMap\>@ element for an NCX file.
mkNavMap :: AnchorFileMap -> NavList -> Markup
mkNavMap db items = el "navMap" $ mapM_ (mkNavPoint db) items

-- Generate a @\<navPoint\>@ element for an NCX file.
mkNavPoint :: AnchorFileMap -> NavListItem -> Markup
mkNavPoint db (NavListItem anchor title subitems) =
  let mkNcxAnchorID = textValue . T.append "ncx-" . internalAnchorID
  in el "navPoint" ! attr "id" (mkNcxAnchorID anchor) $ do
       el "navLabel" (el "text" (string (concatMap plain title)))
       leaf "content" !
         attr "src" (textValue (internalAnchorTarget db anchor))
       mapM_ (mkNavPoint db) subitems
