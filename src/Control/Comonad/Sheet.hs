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
   ( module Control.Comonad.Sheet.Names
   , module Control.Comonad.Sheet.Manipulate
   , module Control.Comonad.Sheet.Indexed
   , module Control.Comonad.Sheet.Reference

   , module Data.Functor.Nested
   , module Data.List.Indexed
   , module Data.Stream.Tape
   , module Data.Numeric.Witness.Peano

   , module Control.Comonad
   , module Data.Distributive
   , module Data.Numeric.Function

   , evaluate
   , cell , cells
   , sheet , indexedSheet
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
