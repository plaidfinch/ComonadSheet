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
         , DimensionalAs x (t (b -> a)) , AsDimensionalAs x (t (b -> a)) ~ l (b -> a) )
         => a -> x -> t (b -> a)
sheet background functions = insert functions (pure (const background))

indexedSheet :: ( InsertNested l (Nested ts) , Applicative (Nested ts)
                , NestedAs x (Nested ts (b -> a)) , AsNestedAs x (Nested ts (b -> a)) ~ l (b -> a))
                => Coordinate (NestedCount ts) -> a -> x -> Indexed ts (b -> a)
indexedSheet i = (Indexed i .) . sheet
