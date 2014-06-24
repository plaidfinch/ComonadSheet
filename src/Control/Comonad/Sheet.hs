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

evaluate :: (ComonadApply w) => w (w a -> a) -> w a
evaluate fs = fix $ (fs <@>) . duplicate

cell :: (Comonad w, Go r w) => RefList r -> w a -> a
cell = (extract .) . go

cells :: (Traversable t, Comonad w, Go r w) => t (RefList r) -> w a -> t a
cells = traverse cell

sheet :: ( InsertNested l (Nested ts) , Applicative (Nested ts)
         , DimensionalAs x (Nested ts a) , AsDimensionalAs x (Nested ts a) ~ l a )
         => a -> x -> Nested ts a
sheet background functions = insert functions (pure background)

indexedSheet :: ( InsertNested l (Nested ts) , Applicative (Nested ts)
                , DimensionalAs x (Nested ts a) , AsDimensionalAs x (Nested ts a) ~ l a)
                => Coordinate (NestedCount ts) -> a -> x -> Indexed ts a
indexedSheet i = (Indexed i .) . sheet
