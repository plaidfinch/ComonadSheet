{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds  #-}

module Unchecked where

import Generic
import Reference
import Nested
import Indexed

import Data.Function
import Control.Comonad
import Control.Applicative
import Data.Traversable

evaluate :: (ComonadApply w) => w (w a -> a) -> w a
evaluate fs = fix $ (fs <@>) . duplicate

cell :: (Comonad w, Go r w) => RefList r -> w a -> a
cell = (extract .) . go

cells :: (Traversable t, Comonad w, Go r w) => t (RefList r) -> w a -> t a
cells = traverse cell

sheet :: ( InsertNested l t , Applicative t
         , DimensionalAs x (t a) , AsDimensionalAs x (t a) ~ l a )
         => a -> x -> t a
sheet background functions = insert functions (pure background)

indexedSheet :: ( InsertNested l (Nested ts) , Applicative (Nested ts)
                , NestedAs x (Nested ts a) , AsNestedAs x (Nested ts a) ~ l a)
                => Coordinate (NestedCount ts) -> a -> x -> Indexed ts a
indexedSheet i = (Indexed i .) . sheet
