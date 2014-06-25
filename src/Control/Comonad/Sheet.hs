{- |
Module      :  Control.Comonad.Sheet
Description :  A library for expressing "spreadsheet-like" computations with absolute and relative references, using fixed-points of n-dimensional comonads.
Copyright   :  Copyright (c) 2014 Kenneth Foner

Maintainer  :  kenneth.foner@gmail.com
Stability   :  experimental
Portability :  non-portable

This module re-exports the various submodules and other dependencies necessary to write comonadic spreadsheet-like
computations using the ComonadSheet library.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE TypeFamilies     #-}

module Control.Comonad.Sheet
   ( evaluate
   , cell , cells
   , sheet , indexedSheet
     -- Names for the relevant aspects of some smaller dimensions.
   , module Control.Comonad.Sheet.Names
     -- Generic functions for manipulating multi-dimensional comonadic spreadsheets.
   , module Control.Comonad.Sheet.Manipulate
     -- Adds absolute position to n-dimensional comonadic spreadsheets.
   , module Control.Comonad.Sheet.Indexed
     -- Relative and absolute references to locations in arbitrary-dimensional sheets.
   , module Control.Comonad.Sheet.Reference

     -- The 'Nested' type enables us to abstract over dimensionality. For instance, a 2-dimensional sheet of integers
     -- is represented by a @Nested (Nest (Flat Tape) Tape) Int@.
   , module Data.Functor.Nested
     -- Counted and conic lists, used for representing references.
   , module Data.List.Indexed
     -- The 'Tape' is the base type we use to construct sheets. A 'Tape' is a both-ways-infinite stream, like a Turing
     -- machine's tape.
   , module Data.Stream.Tape
     -- Peano numerals linked to type-level indices.
   , module Data.Numeric.Witness.Peano

     -- Comonads form the basis of sheet evaluation.
   , module Control.Comonad
     -- Distributivity enables composition of comonads.
   , module Data.Distributive
     -- Numeric instances for functions gives us, for functions @f, g :: Num b => a -> b@, e.g.
     -- @f + g == \x -> f x + g x@. This enables concise syntax for specifying numeric cells in sheets.
   , module Data.Numeric.Function
   ) where

import Control.Comonad.Sheet.Names
import Control.Comonad.Sheet.Manipulate
import Control.Comonad.Sheet.Indexed
import Control.Comonad.Sheet.Reference

import Data.Functor.Nested
import Data.List.Indexed
import Data.Stream.Tape
import Data.Numeric.Witness.Peano

import Control.Comonad
import Control.Applicative
import Data.Distributive
import Data.Traversable
import Data.Numeric.Function
import Data.Function

-- | Take a container of functions referencing containers of values and return the fixed-point: a container of values.
evaluate :: (ComonadApply w) => w (w a -> a) -> w a
evaluate fs = fix $ (fs <@>) . duplicate

-- | Given a relative or absolute position, extract from a sheet the value at that location.
cell :: (Comonad w, Go r w) => RefList r -> w a -> a
cell = (extract .) . go

-- | Given a list of relative or absolute positions, extract from a sheet the values at those locations.
cells :: (Traversable t, Comonad w, Go r w) => t (RefList r) -> w a -> t a
cells = traverse cell

-- | Given a default value and an insertable container of values, construct a sheet containing those values.
sheet :: ( InsertNested l (Nested ts) , Applicative (Nested ts)
         , DimensionalAs x (Nested ts a) , AsDimensionalAs x (Nested ts a) ~ l a )
         => a -> x -> Nested ts a
sheet background functions = insert functions (pure background)

-- | Given an origin index, a default value, and an insertable container of values, construct an indexed sheet.
indexedSheet :: ( InsertNested l (Nested ts) , Applicative (Nested ts)
                , DimensionalAs x (Nested ts a) , AsDimensionalAs x (Nested ts a) ~ l a)
                => Coordinate (NestedCount ts) -> a -> x -> Indexed ts a
indexedSheet i = (Indexed i .) . sheet
