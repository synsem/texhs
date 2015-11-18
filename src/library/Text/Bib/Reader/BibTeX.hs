{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.Bib.Reader.BibTeX
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  portable
--
-- BibTeX parser.
----------------------------------------------------------------------

module Text.Bib.Reader.BibTeX
  ( -- * Types
    BibDB
  , BibEntry(..)
  , FieldValue(..)
    -- * Parser
  , parseBibTeX
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Text.Parsec
import Text.Parsec.Text (Parser)


-------------------- Types

-- | A BibTeX database consists of a list of entries.
type BibDB = [BibEntry]

-- | An entry in a BibTeX database.
--
-- For simplicity, we also treat free comments as BibEntries.
data BibEntry
  = Reference               -- \@Article, \@Book etc.
    { refType :: Text
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
  | PlainField Text
  | ComposedField [FieldValue]  -- for '#' concatenated fields
  deriving (Eq, Show)


-------------------- Constants

-- A whitelist of special characters that are allowed
-- in citation keys, entry type names and field keys.
--
-- Allowed characters: @\@!?*-:()[]@.
safeSpecialChars :: String
safeSpecialChars = "@!?*-:()[]"


-------------------- Parsers

-- | Parse a BibTeX file.
parseBibTeX :: String -> Text -> Either ParseError BibDB
parseBibTeX = parse bibdb

-- Top-level BibTeX parser.
--
-- A BibTeX file consists of bibentries and free comments.
bibdb :: Parser BibDB
bibdb = spaces *> many (bibentry <|> freeComment) <* eof

-- Any text outside an entry.
freeComment :: Parser BibEntry
freeComment = FreeComment . T.stripEnd . T.pack <$> many1 (noneOf "@")

-- A single BibTeX entry.
bibentry :: Parser BibEntry
bibentry = do
  etype <- T.toLower <$> (char '@' *> entrytype)
  case etype of
   "string" -> uncurry Abbrev <$> braced bibfield
   "preamble" -> Preamble <$> braced fieldvalue
   "comment" -> Comment <$> braced fieldvalue
   _ -> Reference etype <$> braced ((:) <$> citekey <*> bibfields)

-- An identifier for an entry.
--
-- Examples: @Article@, @Book@, @String@.
--
-- The identifier is delimited by an open brace to the right
-- and thus must not contain this character ('{').
-- See 'keystring' for a description of allowed identifiers.
entrytype :: Parser Text
entrytype = keystring

-- A key for entry fields.
--
-- The identifier is delimited by an equals sign to the right
-- and thus must not contain this character ('=').
-- See 'keystring' for a description of allowed identifiers.
fieldkey :: Parser Text
fieldkey = keystring

-- A key for 'Reference' entries.
--
-- The identifier is delimited by a comma to the right
-- and thus must not contain this character (',').
-- See 'keystring' for a description of allowed identifiers.
citekey :: Parser (Text, FieldValue)
citekey = (,) "citekey" . PlainField <$> keystring <* char ',' <* spaces

-- A key string.
--
-- A key string must be non-empty and consist of letters, digits
-- and a small set of special characters (see 'safeSpecialChars').
keystring :: Parser Text
keystring = T.pack <$> many1 (alphaNum <|> oneOf safeSpecialChars) <* spaces

-- Fields are separated by commas.
bibfields :: Parser [(Text, FieldValue)]
bibfields = sepEndBy bibfield (char ',' <* spaces)

-- A field consists of a key and a value.
bibfield :: Parser (Text, FieldValue)
bibfield = (,) <$> (fieldkey <* char '=' <* spaces) <*> fieldvalue

-- A field value.
fieldvalue :: Parser FieldValue
fieldvalue = BracedField . T.strip <$> braced bracedText <|>
             QuotedField . T.strip <$> quoted quotedText <|>
             PlainField . T.strip <$> plainText

-- Braced fields must contain brace-balanced text.
-- Quotation marks need not be escaped or balanced.
bracedText :: Parser Text
bracedText = T.concat <$> many ((T.pack <$> (many1 (noneOf "{}"))) <|> withBraces bracedText)

-- Quoted fields must not contain unescaped quotes.
quotedText :: Parser Text
quotedText = (T.pack . concat) <$> many (count 1 (noneOf ('\\':'"':[])) <|> escapedChar)

-- Any content up to a field or entry delimiter.
--
-- Field delimiter: comma.
-- Entry delimiter: closing brace.
plainText :: Parser Text
plainText = T.pack <$> many (noneOf ",}")

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
