{-# LANGUAGE GADTs, RankNTypes, StandaloneDeriving #-}

module Checked where

import Control.Applicative.Free
import Data.Traversable ( Traversable , traverse )
import Data.Set ( Set , empty , insert )

-- | A 'CellRef' is a reference of type r from a container of things of type b, which results in something of type v. When dereferenced, a plain 'CellRef' will always give a result of type b, but used in a free applicative functor, v varies over the result type of the expression.
data CellRef r b v where
   Ref :: r -> CellRef r b b
deriving instance (Show r) => Show (CellRef r b v)

-- | A 'CellExpr' is the free applicative functor over 'CellRefs'. A 'CellExpr' is an expression using references of type r to a container of things of type b, whose result is something of type v.
type CellExpr r b v = Ap (CellRef r b) v

-- | An 'Extract' is a synonym for a function which knows how to take something out of a structure using an index, but is existentially prohibited from thinking about the item it extracts.
type Extract r f = forall x. r -> f x -> x

-- | Returns the set of all references in a 'CellExpr'.
references :: Ord r => CellExpr r b v -> Set r
references (Pure _)       = empty
references (Ap (Ref r) x) = insert r (references x)

-- | Given an appropriate method of dereferencing and a 'CellExpr', returns the function from a structure to a value which is represented by a 'CellExpr'.
runCell :: Extract r f -> CellExpr r b v -> f b -> v
runCell f = runAp $ \(Ref r) -> f r

-- | Constructs a 'CellExpr' which evaluates to whatever is at index r.
cell :: r -> CellExpr r b b
cell = liftAp . Ref

-- | Constructs a 'CellExpr' which evaluates to the list of things referenced by the references in the indices given.
cells :: Traversable f => f r -> CellExpr r b (f b)
cells = traverse cell
