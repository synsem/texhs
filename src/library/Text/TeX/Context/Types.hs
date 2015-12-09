----------------------------------------------------------------------
-- |
-- Module      :  Text.TeX.Context.Types
-- Copyright   :  (c) Mathias Schenner 2015,
--                (c) Language Science Press 2015.
-- License     :  GPL-3
--
-- Maintainer  :  mathias.schenner@langsci-press.org
-- Stability   :  experimental
-- Portability :  GHC
--
-- Types and traversal functions for TeX contexts
----------------------------------------------------------------------

module Text.TeX.Context.Types
  ( -- * Errors
    TeXDocError(..)
  , ThrowsError
    -- * Contexts
  , TeXContext
  , Context(..)
  , pureTeXContext
    -- * Context traversal
    -- ** Types
  , TeXStep
  , TeXStepRes
    -- ** Errors
  , failStep
    -- ** Vertical movement
  , down
  , up
  , intoCmdArg
    -- ** Extractors
  , getHead
    -- ** Manipulate context
  , unconsFocus
  , resetParents
    -- ** Predicates
  , testHead
  , testHeadErr
  , testAtom
  , testEof
  , testEog
  ) where


import Text.TeX.Parser.Types


---------- Error types

-- | Types that may throw a 'TeXDocError'.
type ThrowsError = Either [TeXDocError]

-- | Error during traversal of a 'TeX' value.
data TeXDocError
  = Unexpected TeXAtom
  | EndOfGroup
  | NoParent
  deriving (Eq, Show)


---------- TeX Context

-- | A 'TeXContext' represents a state in the traversal of the 'TeX' AST.
type TeXContext = Context TeXAtom

-- | A Context represents a state during the traversal of a 'TeX'-like AST.
-- It consists of the current focus and a list of future focus
-- values (originally the right siblings of focus parents).
-- Visited nodes (left siblings) are dropped during traversal.
data Context a = Context
  [a]     -- @focus@: current focus
  [[a]]   -- @parents@: list of future focus values

-- | Create a new 'TeXContext' from the provided focus with no parents.
pureTeXContext :: TeX -> TeXContext
pureTeXContext xs = Context xs []


---------- TeX Context traversal

-- | A computation on a 'TeXContext' that may throw a 'TeXDocError'.
type TeXStep = TeXContext -> Either TeXDocError TeXContext

-- | A 'TeXStep' that returns a result.
type TeXStepRes a = TeXContext -> Either TeXDocError (a, TeXContext)

-- | A step that always fails.
failStep :: TeXStep
failStep (Context (x:_) _) = Left (Unexpected x)
failStep (Context [] _) = Left EndOfGroup

-- | Descend into a group.
down :: TeXStep
down (Context (Group _ _ body:xs) nxt) = return (Context body (xs:nxt))
down ctx = failStep ctx

-- | Drop focus and climb up one level.
up :: TeXStep
up (Context _ (xs:xss)) = return (Context xs xss)
up (Context _ []) = Left NoParent

-- | Descend into the first mandatory argument of a command
-- (all other arguments are dropped).
intoCmdArg :: TeXStep
intoCmdArg (Context ((Command _ args):xs) nxt) =
  return (Context (getOblArg 0 args) (xs:nxt))
intoCmdArg ctx = failStep ctx

-- | Extract head of focus.
getHead :: TeXContext -> Either TeXDocError TeXAtom
getHead (Context (x:_) _) = return x
getHead (Context [] _) = Left EndOfGroup

-- | Split head of focus from remaining context.
unconsFocus :: TeXStepRes TeXAtom
unconsFocus (Context (x:xs) nxt) = return (x, Context xs nxt)
unconsFocus (Context [] _) = Left EndOfGroup

-- | Reset parents in context.
resetParents :: TeXStep
resetParents (Context foc _) = return (Context foc [])

-- | Lift a 'TeXAtom' property to a 'TeXContext' property
-- by applying it to the head of the focus.
testHead :: (TeXAtom -> Bool) -> TeXContext -> Bool
testHead p (Context (x:_) _) = p x
testHead _ _ = False

-- | Apply a 'TeXAtom' property to the head of the focus.
--
-- Like 'testHead' but throws a 'TeXDocError' if the property fails.
testHeadErr :: (TeXAtom -> Bool) -> TeXStep
testHeadErr p ctx = if testHead p ctx then return ctx else failStep ctx

-- | Apply a 'TeXAtom' property and fail if it is not satisfied.
testAtom :: (TeXAtom -> Bool) -> TeXAtom -> Either TeXDocError TeXAtom
testAtom p x = if p x then return x else Left (Unexpected x)

-- | Succeed if context is empty
-- (i.e. if the focus is empty and there are no parents).
testEof :: TeXStep
testEof ctx@(Context foc nxt) =
  let trailer = concat (foc:nxt)
  in if null trailer
     then return ctx
     else Left (Unexpected (head trailer))

-- | Succeed if focus is empty
-- (i.e. if we hit an end of group).
testEog :: TeXStep
testEog ctx@(Context [] _) = return ctx
testEog (Context (x:_) _) = Left (Unexpected x)
