{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.Bib.Reader.BibTeX.Structure
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  portable
--
-- BibTeX parser, layer 1:
-- extract BibTeX entries and resolve \@string macros.
-- Field values are not interpreted, see
-- "Text.Bib.Reader.BibTeX.Reference" instead.
----------------------------------------------------------------------

module Text.Bib.Reader.BibTeX.Structure
  ( -- * Types
    BibTeXDB
  , BibTeXEntry(..)
  , FieldValue(..)
    -- * Predicates for BibTeX entry types
  , isReference
  , isAbbrev
  , isPreamble
    -- * Accessors for BibTeX entries
  , getField
  , unwrapPreamble
  , unwrapFieldValue
    -- * Parser
  , parseBibTeX
  ) where

import Data.Char (isDigit)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec


-------------------- BibTeX types

-- | A BibTeX database consists of a list of entries.
type BibTeXDB = [BibTeXEntry]

-- | An entry in a BibTeX database.
--
-- For simplicity, we also treat free comments as BibEntries.
data BibTeXEntry
  = Reference               -- \@Article, \@Book etc.
    { refType :: Text
    , refKey :: Text
    , refFields :: [(Text, FieldValue)]
    }
  | Abbrev                  -- \@String
    { abbrKey :: Text
    , abbrVal :: FieldValue
    }
  | Preamble FieldValue     -- \@Preamble
  | Comment FieldValue      -- \@Comment
  | FreeComment Text        -- free text outside of any entry
  deriving (Eq, Show)

-- | Textual data contained in a field of an entry.
data FieldValue
  = BracedField Text
  | QuotedField Text
  | PlainField Text FieldValue  -- second param is abbrev-expanded value
  | ComposedField [FieldValue]  -- for '#' concatenated fields
  deriving (Eq, Show)


-------------------- Predicates for BibTeX entry types

-- | Test whether a 'BibTeXEntry' is a 'Reference'.
isReference :: BibTeXEntry -> Bool
isReference (Reference{}) = True
isReference _ = False

-- | Test whether a 'BibTeXEntry' is an 'Abbrev'.
isAbbrev :: BibTeXEntry -> Bool
isAbbrev (Abbrev{}) = True
isAbbrev _ = False

-- | Test whether a 'BibTeXEntry' is a 'Preamble'.
isPreamble :: BibTeXEntry -> Bool
isPreamble (Preamble{}) = True
isPreamble _ = False


-------------------- Accessors for BibTeX entries

-- | Extract the value of a field by its name in a 'Reference' entry.
getField :: Text -> BibTeXEntry -> Maybe FieldValue
getField fkey (Reference _ _ fs) = lookup fkey fs
getField _ _ = Nothing

-- | Extract field value of a 'Preamble' entry.
--
-- Throws an exception if applied to a non-Preamble entry.
unwrapPreamble :: BibTeXEntry -> Maybe Text
unwrapPreamble (Preamble v) = return (unwrapFieldValue v)
unwrapPreamble _ = Nothing

-- | Extract textual data from a BibTeX field value.
unwrapFieldValue :: FieldValue -> Text
unwrapFieldValue (BracedField v) = v
unwrapFieldValue (QuotedField v) = v
unwrapFieldValue (PlainField _ v) = unwrapFieldValue v
unwrapFieldValue (ComposedField vs) = T.concat (map unwrapFieldValue vs)


-------------------- Constants

-- A whitelist of special characters that are allowed
-- in citation keys, entry type names and field keys.
--
-- Allowed characters: @\@.:;!?+-*/[]@.
safeSpecialChars :: String
safeSpecialChars = "@.:;!?+-*/[]"


-------------------- Parser types

-- Main BibTeX parser type.
--
-- Stream type: Text
-- User state: AbbrevMap
type Parser = Parsec Text AbbrevMap

-- A map of currently defined BibTeX macros
-- (i.e. @\@string@ abbreviations).
type AbbrevMap = Map Text FieldValue

-- Default AbbrevMap (macro map).
--
-- This map includes standard macros pre-defined by BibTeX.
-- In particular, three-letter abbreviations of months are mapped
-- to integers, as in: @\@string{ "jan" = {1} }@.
defaultAbbrevMap :: AbbrevMap
defaultAbbrevMap = M.fromList $ zip monthNames monthNums
  where
    monthNames = ["jan","feb","mar","apr","may","jun"
                 ,"jul","aug","sep","oct","nov","dec"]
    monthNums = map (BracedField . T.pack . show) ([1..] :: [Int])


-------------------- Parsers

-- | Parse a BibTeX file.
parseBibTeX :: String -> Text -> Either ParseError BibTeXDB
parseBibTeX = runParser bibdb defaultAbbrevMap

-- Top-level BibTeX parser.
--
-- A BibTeX file consists of bibentries and free comments.
bibdb :: Parser BibTeXDB
bibdb = spaces *> many (bibentry <|> freeComment) <* eof

-- Any text outside an entry.
freeComment :: Parser BibTeXEntry
freeComment = FreeComment . T.stripEnd . T.pack <$> many1 (noneOf "@")

-- A single BibTeX entry.
bibentry :: Parser BibTeXEntry
bibentry = do
  etype <- T.toLower <$> (char '@' *> entrytype)
  case etype of
   "string" -> abbrev =<< parenOrBraced bibfield
   "preamble" -> Preamble <$> parenOrBraced fieldvalue
   "comment" -> Comment <$> parenOrBraced fieldvalue
   _ -> uncurry (Reference etype)
          <$> parenOrBraced ((,) <$> citekey <*> bibfields)

-- Register a macro (abbreviation) in the parser state
-- and return it as a BibTeXEntry.
abbrev :: (Text, FieldValue) -> Parser BibTeXEntry
abbrev (k, v) = Abbrev k v <$ modifyState (M.insert k v)

-- An identifier for an entry (aka entry type).
--
-- Examples: @Article@, @Book@, @String@.
--
-- The identifier is delimited by an open brace to the right
-- and thus must not contain this character ('{').
-- See 'keystring' for a description of allowed identifiers.
entrytype :: Parser Text
entrytype = keystring

-- A key for entry fields (aka field name).
--
-- The identifier is delimited by an equals sign to the right
-- and thus must not contain this character ('=').
-- See 'keystring' for a description of allowed identifiers.
fieldkey :: Parser Text
fieldkey = keystring

-- A key for 'Reference' entries (aka cite key, entry key).
--
-- The identifier is delimited by a comma to the right
-- and thus must not contain this character (',').
-- See 'keystring' for a description of allowed identifiers.
citekey :: Parser Text
citekey = citekeystring <* char ',' <* spaces

-- A key string (used for field names).
--
-- A key string must be non-empty and consist of letters, digits
-- and a small set of special characters (see 'safeSpecialChars').
-- It may consist exclusively of special characters.
--
-- A key string must not start with a digit (unlike a citekey
-- string). BibTeX (biber-2.2) throws an error if a key string does
-- start with a digit. By contrast, we currently simply drop any
-- leading digits (even if we end up with an empty string). This
-- ensures that there are no macro names that start with digits.
keystring :: Parser Text
keystring = T.dropWhile isDigit <$> citekeystring

-- A citekey string (used for cite keys).
--
-- Similar to keystring but may start with a digit.
citekeystring :: Parser Text
citekeystring = T.pack <$>
  many1 (alphaNum <|> oneOf safeSpecialChars) <* spaces

-- Fields are separated by commas.
bibfields :: Parser [(Text, FieldValue)]
bibfields = bibfield `sepEndBy` (char ',' <* spaces)

-- A field consists of a key and a value.
--
-- Field names (including macro names) are case-insensitive,
-- so we normalize them to lower case.
bibfield :: Parser (Text, FieldValue)
bibfield = (,)
  <$> (T.toLower <$> fieldkey <* char '=' <* spaces)
  <*> fieldvalue

-- A field value, possibly composed of several subfields
-- separated by a number sign ('#').
fieldvalue :: Parser FieldValue
fieldvalue = packField <$> (simplefieldvalue `sepBy` (char '#' <* spaces))
  where
    -- Wrap non-singleton fields in a 'ComposedField'.
    packField :: [FieldValue] -> FieldValue
    packField [f] = f
    packField fs = ComposedField fs

-- A simple (non-composed) field value.
simplefieldvalue :: Parser FieldValue
simplefieldvalue =
  BracedField <$> braced bracedText <|>
  QuotedField <$> quoted quotedText <|>
  plainField =<< (T.strip <$> plainText)

-- Construct a 'PlainField' from a textual key.
--
-- Unless the key starts with a digit,
-- try to expand it using the current macro map.
plainField :: Text -> Parser FieldValue
plainField key = case T.uncons key of
  Just (c,_)
    | isDigit c -> return (plainFieldNoExpand key)
    | otherwise -> plainFieldExpand key
  Nothing -> return (plainFieldNoExpand key)

-- Construct a 'PlainField' from a textual key,
-- trying to expand it.
--
-- Tries to expand the textual key based on the currently defined
-- macro map. If the lookup fails, an empty braced field is used.
plainFieldExpand :: Text -> Parser FieldValue
plainFieldExpand key = PlainField key
  . maybe (BracedField T.empty) id
  . M.lookup (T.toLower key) <$> getState

-- Construct a 'PlainField' from a textual key,
-- without trying to expand it.
plainFieldNoExpand :: Text -> FieldValue
plainFieldNoExpand key = PlainField key (BracedField key)

-- Braced fields must contain brace-balanced text.
-- Quotation marks need not be escaped or balanced.
bracedText :: Parser Text
bracedText = T.concat <$> many ((T.pack <$> (many1 (noneOf "{}"))) <|> withBraces bracedText)

-- Quoted fields must not contain unescaped quotes.
quotedText :: Parser Text
quotedText = (T.pack . concat) <$> many (count 1 (noneOf ('\\':'"':[])) <|> escapedChar)

-- Any content up to a field or entry delimiter.
--
-- Inner field delimiter: number sign.
-- Outer field delimiter: comma.
-- Entry delimiter: closing brace.
plainText :: Parser Text
plainText = T.pack <$> many (noneOf "#,}") <* spaces

-- Any backslash-escaped character.
escapedChar :: Parser String
escapedChar = (:) <$> char '\\' <*> count 1 anyChar


-------------------- Parsing helpers

-- Like 'braced' but keep the braces in the result.
withBraces :: Parser Text -> Parser Text
withBraces p = wrapBraces <$> braced p
  where
    wrapBraces t = T.concat ["{", t, "}"]

-- Apply parser between braces and skip trailing whitespace.
braced :: Parser a -> Parser a
braced p = between (char '{') (char '}') p <* spaces

-- Apply parser between double quotes and skip trailing whitespace.
quoted :: Parser a -> Parser a
quoted p = between (char '"') (char '"') p <* spaces

-- Apply parser between braces or parentheses and skip trailing whitespace.
--
-- Mismatching delimiters (e.g. opening brace and closing parens) are allowed
-- (but biber-2.2 issues a warning in such cases).
parenOrBraced :: Parser a -> Parser a
parenOrBraced p = between (oneOf "{(") (oneOf "})") p <* spaces
