{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.Doc.Writer.Core
-- Copyright   :  2015-2016 Mathias Schenner,
--                2015-2016 Language Science Press.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  GHC
--
-- Common types and utilities for writers.
----------------------------------------------------------------------

module Text.Doc.Writer.Core
 ( -- * XML generation
   el
 , leaf
 , attr
 , (<!>)
 , withXmlDeclaration
 , withXhtml11Doctype
   -- * Lifted monoid functions
 , (<+>)
 , ($<>)
 , (<>$)
 , memptyR
 , mconcatR
 , foldMapR
 , unlessR
 ) where

import Control.Applicative
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Text.Blaze (Markup, Attribute, AttributeValue, textTag, (!), preEscapedText)
import Text.Blaze.Internal (Attributable, customLeaf, customParent, attribute)


---------- Generate XML Markup

-- | Create element with content from tag name.
el :: Text -> Markup -> Markup
el = customParent . textTag

-- | Create self-closing empty element from tag name.
leaf :: Text -> Markup
leaf = flip customLeaf True . textTag

-- | Create attribute from name and value.
attr :: Text -> AttributeValue -> Attribute
attr n = attribute (textTag n) (textTag (" " <> n <> "=\""))

infixl 5 <!>

-- | Lifted attribute setter.
(<!>) :: (Functor f, Attributable h) => f h -> Attribute -> f h
(<!>) = flip (fmap . flip (!))

-- | Prefix XML declaration.
withXmlDeclaration :: Markup -> Markup
withXmlDeclaration =
  (preEscapedText "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" <>)

-- | Prefix XHTML 1.1 doctype declaration.
withXhtml11Doctype :: Markup -> Markup
withXhtml11Doctype = (preEscapedText xhtml11Doctype <>)
  where
    xhtml11Doctype = T.unlines
      [ "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\""
      , "    \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">" ]


---------- Lifted monoid functions

-- These are mainly intended for monads atop 'Markup'.

infixl 5 <+>
infixr 5 $<>
infixl 5 <>$

-- | Lifted mappend.
(<+>) :: (Applicative f, Monoid a) => f a -> f a -> f a
(<+>) = liftA2 mappend

-- | Lifted mappend, apply pure to left value.
($<>) :: (Functor f, Monoid a) => a -> f a -> f a
($<>) = fmap . mappend

-- | Lifted mappend, apply pure to right value.
(<>$) :: (Functor f, Monoid a) => f a -> a -> f a
(<>$) = flip (fmap . flip mappend)

-- | Lifted mempty.
memptyR :: (Applicative f, Monoid a) => f a
memptyR = pure mempty

-- | Lifted mconcat.
mconcatR :: (Applicative f, Monoid a) => [f a] -> f a
mconcatR = foldr (<+>) memptyR

-- | Specialized foldMap.
foldMapR :: (Applicative f, Monoid b) => (a -> f b) -> [a] -> f b
foldMapR f = mconcatR . map f

-- | Conditional insertion of a monoid value.
unlessR :: (Applicative f, Monoid a) => Bool -> f a -> f a
unlessR True = const memptyR
unlessR False = id
