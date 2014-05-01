{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}

module Checked where

import Control.Applicative.Free
import Data.Traversable ( Traversable , traverse )

-- | A 'CellRef' is a reference of type r from a container of things of type a, which results in
--   something of type b. When dereferenced, a plain 'CellRef' will always give a result of type a,
--   but used in a free applicative functor, b varies over the result type of the expression.
data CellRef r a b where
   CellRef :: r -> CellRef r a a
deriving instance (Show r) => Show (CellRef r a b)

-- | A 'CellExpr' is the free applicative functor over 'CellRefs'. A 'CellExpr' is an expression using
--   references of type r to a container of things of type a, whose result is something of type b.
type CellExpr r a b = Ap (CellRef r a) b

-- | Returns the list of all references in a 'CellExpr'.
references :: CellExpr r a b -> [r]
references (Pure _)           = []
references (Ap (CellRef r) x) = r : references x

-- | An 'Extract' is a synonym for a function which knows how to take something out of a structure, but is 
--   existentially prohibited from thinking about the item it extracts.
type Extract f = forall x. f x -> x

-- | Given an appropriate method of dereferencing and a 'CellExpr', returns the function from a structure 
--   to a value which is represented by a 'CellExpr'.
runCell :: (r -> Extract f) -> CellExpr r a b -> f a -> b
runCell f = runAp $ \(CellRef r) -> f r

-- | Constructs a 'CellExpr' which evaluates to whatever is at index r.
cell :: r -> CellExpr r b b
cell = liftAp . CellRef

-- | Constructs a 'CellExpr' which evaluates to a Traversable of the referents of the references given.
cells :: Traversable f => f r -> CellExpr r a (f a)
cells = traverse cell
