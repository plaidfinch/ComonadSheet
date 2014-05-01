{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}

module Checked where

import Control.Applicative.Free
import Data.Traversable ( Traversable , traverse )

-- | A 'CellRef' is a reference of type r from a container of things of type b, which results in
--   something of type v. When dereferenced, a plain 'CellRef' will always give a result of type b,
--   but used in a free applicative functor, v varies over the result type of the expression.
data CellRef r b v where
   CellRef :: r -> CellRef r b b
deriving instance (Show r) => Show (CellRef r b v)

-- | A 'CellExpr' is the free applicative functor over 'CellRefs'. A 'CellExpr' is an expression using
--   references of type r to a container of things of type b, whose result is something of type v.
type CellExpr r b v = Ap (CellRef r b) v

-- | Returns the list of all references in a 'CellExpr'.
references :: CellExpr r b v -> [r]
references (Pure _)           = []
references (Ap (CellRef r) x) = r : references x

-- | An 'Extract' is a synonym for a function which knows how to take something out of a structure using
--   a reference r of some sort, but is existentially prohibited from thinking about the item it extracts.
type Extract r f = forall x. r -> f x -> x

-- | Given an appropriate method of dereferencing and a 'CellExpr', returns the function from a structure 
--   to a value which is represented by a 'CellExpr'.
runCell :: Extract r f -> CellExpr r b v -> f b -> v
runCell f = runAp $ \(CellRef r) -> f r

-- | Constructs a 'CellExpr' which evaluates to whatever is at index r.
cell :: r -> CellExpr r b b
cell = liftAp . CellRef

-- | Constructs a 'CellExpr' which evaluates to a Traversable of the referents of the references given.
cells :: Traversable f => f r -> CellExpr r b (f b)
cells = traverse cell
