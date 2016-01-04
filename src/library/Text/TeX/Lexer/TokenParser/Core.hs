{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.TeX.Lexer.TokenParser.Core
-- Copyright   :  2015-2016 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  GHC
--
-- Low-level TeX lexers and lifted Parsec functions for TeX lexing.
----------------------------------------------------------------------

module Text.TeX.Lexer.TokenParser.Core
  ( -- * Lexer type
    LexerT
  , runLexer
  , runLexerIO
  , HandleTeXIO(..)
    -- * Fundamental lexers
  , satisfyToken
  , satisfyChar
  , satisfyCharCC
    -- * Stream modifications
  , prependTokens
  , prependString
    -- * Parsec re-exports
    -- ** State
  , getState
  , modifyState
    -- ** Error
  , (<?>)
  , unexpected
    -- ** Combinator
  , (<|>)
  , between
  , choice
  , count
  , eof
  , lookAhead
  , many
  , many1
  , manyTill
  , option
  , optionMaybe
  , optional
  , try
  ) where

#if MIN_VERSION_base(4,8,0)
import Control.Applicative (Alternative)
#else
import Control.Applicative (Alternative, Applicative, (<$>))
#endif
import Control.Monad (MonadPlus)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Time (getZonedTime, formatTime, defaultTimeLocale)
import System.FilePath ((<.>), (</>))
import System.Directory
  (getCurrentDirectory, getPermissions, findFilesWith, readable)
import Text.Parsec (ParsecT, ParseError, tokenPrim)
import qualified Text.Parsec as P
import Text.Parsec.Pos (updatePosChar)

import Text.TeX.Lexer.Catcode (Catcode, hasCatcode)
import Text.TeX.Lexer.Token (Token, getRawChar, isCharSat, hasCC)
import Text.TeX.Lexer.TokenParser.State
  (LexerState, ThrowsError, validate, getCatcodes)


-------------------- Lexer type

-- | A type class for monads that know how to handle requests for
-- certain IO actions that are denoted by primitive TeX commands,
-- like @\\input@ or @\\year@.
class Monad m => HandleTeXIO m where
  handleReadFile :: FilePath -> m String
  handleReadDate :: m String

-- Execute IO actions.
instance HandleTeXIO IO where
  handleReadFile = liftIO . readTeXFile
  handleReadDate = liftIO getZonedTime >>= return .
                   formatTime defaultTimeLocale "%Y-%m-%d %H:%M"

-- Read a TeX file.
-- Appends ".tex" if the provided filename does not exist.
readTeXFile :: FilePath -> IO String
readTeXFile filename = do
  cwd <- getCurrentDirectory
  paths <- concat <$> mapM (findFilesWith (fmap readable . getPermissions) [cwd])
           (map (filename <.>) ["", "tex"])
  case paths of
    (path:_) -> readFile path
    [] -> error $ "cannot read file: " ++ (cwd </> filename)

-- Silently ignore all requests for IO actions.
instance HandleTeXIO Identity where
  handleReadFile _ = return ""
  handleReadDate = return ""

instance HandleTeXIO m => HandleTeXIO (ParsecT s u m) where
  handleReadFile = lift . handleReadFile
  handleReadDate = lift handleReadDate

-- | Lexer for TeX input streams.
newtype LexerT m a = LexerT {
    runLexerT :: TeXParsecT m a
  } deriving (Functor, Applicative, Alternative, Monad, MonadPlus,
              MonadTrans, HandleTeXIO)

-- | Run a TeX lexer on a 'Char' input stream over the Identity monad, ignoring IO requests.
runLexer :: LexerT Identity a -> LexerState -> String -> String -> Either ParseError a
runLexer (LexerT p) st name input = runIdentity (P.runParserT p st name (map Left input))

-- | Run a TeX lexer on a 'Char' input stream over the IO monad, executing IO requests.
runLexerIO :: LexerT IO a -> LexerState -> String -> String -> IO (Either ParseError a)
runLexerIO (LexerT p) st name input = P.runParserT p st name (map Left input)

-- ParsecT specialized for TeX input streams.
type TeXParsecT = ParsecT [CharOrToken] LexerState

-- The input to the lexer is a stream of @CharOrToken@ elements.
-- These are either (1) unparsed and yet unseen raw @Char@ elements
-- (i.e. characters that need to be assigned a catcode or to be
-- assembled into control sequences) or (2) already parsed @Token@
-- elements (e.g. characters with existing catcode assignment, like
-- they are stored in macros). @Token@ elements in this stream are
-- usually the result of macro expansion.

-- NOTE: We are using @Either@ to hold two equally relevant types.
-- In particular, @Left@ values are not to be interpreted as errors.
-- | This is the type of elements of a TeX input stream.
type CharOrToken = Either Char Token


-------------------- Fundamental lexers

-- | Parse a character that both satisfies the provided property
-- and has one of the listed catcodes.
satisfyCharCC :: Monad m => (Char -> Bool) -> [Catcode] -> LexerT m Char
satisfyCharCC p ccs = satisfyCharCCT p ccs <|> satisfyCharCCC p ccs

satisfyCharCCT :: Monad m => (Char -> Bool) -> [Catcode] -> LexerT m Char
satisfyCharCCT p ccs = getRawChar <$> satisfyToken (\t ->
                         isCharSat p t && any (`hasCC` t) ccs)

satisfyCharCCC :: Monad m => (Char -> Bool) -> [Catcode] -> LexerT m Char
satisfyCharCCC p ccs = do
  cctab <- getCatcodes <$> getState
  satisfyChar (\x -> p x && any (\cc -> hasCatcode cc cctab x) ccs)


-- Parse a @Right@ stream element.
-- | Parse a verifying token.
satisfyToken :: Monad m => (Token -> Bool) -> LexerT m Token
satisfyToken p = satisfy (either (const False) p) >>= \(Right t) -> return t

-- Parse a @Left@ stream element.
-- | Parse a verifying character.
satisfyChar :: Monad m => (Char -> Bool) -> LexerT m Char
satisfyChar p = satisfy (either p (const False)) >>= \(Left c) -> return c

-- Fundamental lexer for "CharOrToken" streams.
satisfy :: Monad m => (CharOrToken -> Bool) -> LexerT m CharOrToken
satisfy p = LexerT $ tokenPrim show nextpos test
  where
    nextpos = \pos t _ -> case t of
      Left c -> updatePosChar pos c
      Right _ -> pos -- don't increment position
    test = \t -> if p t then Just t else Nothing


-------------------- Stream modifications

-- | Prepend a list of tokens to the input stream.
prependTokens :: Monad m => [Token] -> LexerT m ()
prependTokens xs = (map Right xs ++) <$> getInput >>= setInput

-- | Prepend a list of chars to the input stream.
prependString :: Monad m => String -> LexerT m ()
prependString xs = (map Left xs ++) <$> getInput >>= setInput


-------------------- Parsec re-exports

---------- lifting helpers

liftP :: (TeXParsecT m a -> TeXParsecT m r) ->
         LexerT m a -> LexerT m r
liftP f (LexerT p) = LexerT (f p)

liftP2 :: (TeXParsecT m a1 -> TeXParsecT m a2 -> TeXParsecT m r) ->
          LexerT m a1 -> LexerT m a2 -> LexerT m r
liftP2 f (LexerT p1) (LexerT p2) = LexerT (f p1 p2)

liftP3 :: (TeXParsecT m a1 -> TeXParsecT m a2 ->
           TeXParsecT m a3 -> TeXParsecT m r) ->
          LexerT m a1 -> LexerT m a2 -> LexerT m a3 -> LexerT m r
liftP3 f (LexerT p1) (LexerT p2) (LexerT p3) = LexerT (f p1 p2 p3)

---------- input

-- | See 'P.getInput' from "Text.Parsec".
getInput :: Monad m => LexerT m [CharOrToken]
getInput = LexerT P.getInput

-- | See 'P.setInput' from "Text.Parsec".
setInput :: Monad m => [CharOrToken] -> LexerT m ()
setInput = LexerT . P.setInput

---------- state

-- | Retrieve the current lexer state.
-- Throws a 'ParseError' if the lexer state is invalid.
--
-- Also see 'P.getState' from "Text.Parsec".
getState :: Monad m => LexerT m LexerState
getState = validate <$> LexerT P.getState >>=
           either (parserFail . show) return

-- | Apply a function to the lexer state.
-- Throws a 'ParseError' if the function fails.
--
-- Also see 'P.modifyState' from "Text.Parsec".
modifyState :: Monad m => (LexerState -> ThrowsError LexerState) -> LexerT m ()
modifyState f = f <$> getState >>=
                either (parserFail . show) (LexerT . P.putState)

---------- error

infix 0 <?>

-- | See 'P.<?>' from "Text.Parsec".
(<?>) :: Monad m => LexerT m a -> String -> LexerT m a
p <?> msg = liftP (P.<?> msg) p

-- | See 'P.unexpected' from "Text.Parsec".
unexpected :: Monad m => String -> LexerT m a
unexpected = LexerT . P.unexpected

-- | See 'P.parserFail' from "Text.Parsec".
parserFail :: Monad m => String -> LexerT m a
parserFail = LexerT . P.parserFail

---------- combinator

infixr 1 <|>

-- | See 'P.<|>' from "Text.Parsec".
(<|>) :: Monad m => LexerT m a -> LexerT m a -> LexerT m a
(<|>) = liftP2 (P.<|>)

-- | See 'P.between' from "Text.Parsec".
between :: Monad m => LexerT m open -> LexerT m close ->
           LexerT m a -> LexerT m a
between = liftP3 P.between

-- | See 'P.choice' from "Text.Parsec".
choice :: Monad m => [LexerT m a] -> LexerT m a
choice = LexerT . P.choice . map runLexerT

-- | See 'P.count' from "Text.Parsec".
count :: Monad m => Int -> LexerT m a -> LexerT m [a]
count = liftP . P.count

-- | See 'P.eof' from "Text.Parsec".
eof :: Monad m => LexerT m ()
eof = LexerT P.eof

-- | See 'P.many' from "Text.Parsec".
many :: Monad m => LexerT m a -> LexerT m [a]
many = liftP P.many

-- | See 'P.many1' from "Text.Parsec".
many1 :: Monad m => LexerT m a -> LexerT m [a]
many1 = liftP P.many1

-- | See 'P.manyTill' from "Text.Parsec".
manyTill :: Monad m => LexerT m a -> LexerT m end -> LexerT m [a]
manyTill = liftP2 P.manyTill

-- | See 'P.option' from "Text.Parsec".
option :: Monad m => a -> LexerT m a -> LexerT m a
option = liftP . P.option

-- | See 'P.optionMaybe' from "Text.Parsec".
optionMaybe :: Monad m => LexerT m a -> LexerT m (Maybe a)
optionMaybe = liftP P.optionMaybe

-- | See 'P.optional' from "Text.Parsec".
optional :: Monad m => LexerT m a -> LexerT m ()
optional = liftP P.optional

-- | See 'P.try' from "Text.Parsec".
try :: Monad m => LexerT m a -> LexerT m a
try = liftP P.try

-- | See 'P.lookAhead' from "Text.Parsec".
lookAhead :: Monad m => LexerT m a -> LexerT m a
lookAhead = liftP P.lookAhead
