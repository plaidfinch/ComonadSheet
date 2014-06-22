{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}

module Checked where

import Control.Applicative
import Control.Applicative.Free
import Data.Traversable ( Traversable , traverse )

-- | A 'CellRef' is a reference of type r from a container of things of type a, which results in
--   something of type b. When dereferenced, a plain 'CellRef' will always give a result of type a,
--   but used in a free applicative functor, b varies over the result type of the expression.
data CellRef r f a b where
   StaticRef  :: r -> CellRef r f a a
   DynamicRef :: (f a -> b) -> CellRef r f a b

-- | A CellExpr is the free applicative functor over 'CellRefs'. A CellExpr is an expression using
--   references of type r to a container of things of type a, whose result is something of type b.
type CellExpr r f a = Ap (CellRef r f a)

-- | Returns the list of all references in a CellExpr.
references :: CellExpr r f a b -> Maybe [r]
references (Pure _)              = Just []
references (Ap (StaticRef  r) x) = (r :) <$> references x
references (Ap (DynamicRef _) _) = Nothing

-- | An 'Extract' is a synonym for a function which knows how to take something out of a structure
--   which may contain things of any type.
type Extract f = forall x. f x -> x

-- | Given an appropriate method of dereferencing and a CellExpr, returns the function from a structure 
--   to a value which is represented by a CellExpr.
runCell :: (r -> Extract f) -> CellExpr r f a b -> f a -> b
runCell e = runAp $ \case StaticRef  r -> e r
                          DynamicRef f -> f

-- | Constructs a CellExpr which evaluates to whatever is at index r.
cell :: r -> CellExpr r f a a
cell = liftAp . StaticRef

-- | Lift a function on a structure to a CellExpr.
liftC :: (f a -> b) -> CellExpr r f a b
liftC = liftAp . DynamicRef

-- | Constructs a CellExpr which evaluates to a Traversable of the referents of the references given.
cells :: Traversable t => t r -> CellExpr r f a (t a)
cells = traverse cell
