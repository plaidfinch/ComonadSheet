{- |
Module      :  Control.Comonad.Sheet.Names
Description :  Names for the relevant aspects of some smaller dimensions (currently up to 4).
Copyright   :  Copyright (c) 2014 Kenneth Foner

Maintainer  :  kenneth.foner@gmail.com
Stability   :  experimental
Portability :  non-portable

This module defines names to be used manipulating n-dimensional sheets. Currently, names are defined for dimensions
of 4 and fewer. Below is a summary of the names currently defined in this module. Template Haskell to define these names for new dimension numbers is coming soon!

=Dimension 1:

   * @Sheet1@ is the type of a 1-dimensional sheet

   * @left@ (negative) and @right@ (positive) are directions

   * @leftBy@ and @rightBy@ define relative position by some integer argument

   * @columnAt@ defines absolute position at a given column

   * @column@ retrieves the current column index

   * @d1@ coerces a 1-or-fewer-dimensional reference to a 1-dimensional reference

=Dimension 2:
   
   * @Sheet2@ is the type of a 2-dimensional sheet

   * @above@ (negative) and @below@ (positive) are directions

   * @aboveBy@ and @belowBy@ define relative position by some integer argument

   * @rowAt@ defines absolute position at a given row

   * @row@ retrieves the current row index

   * @d2@ coerces a 2-or-fewer-dimensional reference to a 2-dimensional reference

=Dimension 3:

   * @Sheet3@ is the type of a 3-dimensional sheet

   * @inward@ (negative) and @outward@ (positive) are directions

   * @inwardBy@ and @outwardBy@ define relative position by some integer argument

   * @levelAt@ defines absolute position at a given level

   * @level@ retrieves the current level index

   * @d3@ coerces a 3-or-fewer-dimensional reference to a 3-dimensional reference

=Dimension 4:

   * @Sheet4@ is the type of a 4-dimensional sheet

   * @ana@ (negative) and @kata@ (positive) are directions

   * @anaBy@ and @kataBy@ define relative position by some integer argument

   * @spaceAt@ defines absolute position at a given space

   * @space@ retrieves the current space index

   * @d4@ coerces a 4-or-fewer-dimensional reference to a 4-dimensional reference

-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module Control.Comonad.Sheet.Names
   ( Sheet1 , here1 , d1 , columnAt , column , rightBy , leftBy , right , left
   , Sheet2 , here2 , d2 , rowAt , row , aboveBy , belowBy , above , below
   , Sheet3 , here3 , d3 , levelAt , level , inwardBy , outwardBy , inward , outward
   , Sheet4 , here4 , d4 , spaceAt , space , anaBy , kataBy , ana , kata
   ) where

import Control.Comonad.Sheet.Reference
import Data.Numeric.Witness.Peano
import Data.Stream.Tape
import Control.Comonad.Sheet.Indexed
import Data.Functor.Nested
import Data.List.Indexed

-- One dimension...

type Rel1 = Relative :-: Nil
type Nat1 = Succ Zero

nat1 :: Natural Nat1
nat1 = reifyNatural

type Sheet1  = Nested  (NestedNTimes Nat1 Tape)
type ISheet1 = Indexed (NestedNTimes Nat1 Tape)

here1 :: RefList Rel1
here1 = Rel 0 :-: ConicNil

d1 :: (CombineRefLists Rel1 x) => RefList x -> RefList (Rel1 & x)
d1 = (here1 &)

columnAt :: Int -> RefList (Absolute :-: Nil)
columnAt = dimensional nat1 . Abs

column :: (Zero < NestedCount ts) => Indexed ts x -> Int
column = getRef . nth Zero . index

rightBy, leftBy :: Int -> RefList Rel1
rightBy = dimensional nat1 . Rel
leftBy = rightBy . negate

right, left :: RefList Rel1
right = rightBy 1
left  = leftBy  1

-- Two dimensions...

type Rel2 = Relative :-: Rel1
type Nat2 = Succ Nat1

nat2 :: Natural Nat2
nat2 = reifyNatural

type Sheet2  = Nested  (NestedNTimes Nat2 Tape)
type ISheet2 = Indexed (NestedNTimes Nat2 Tape)

here2 :: RefList Rel2
here2 = Rel 0 :-: here1

d2 :: (CombineRefLists Rel2 x) => RefList x -> RefList (Rel2 & x)
d2 = (here2 &)

rowAt :: Int -> RefList (Tack Absolute Rel1)
rowAt = dimensional nat2 . Abs

row :: (Nat1 < NestedCount ts) => Indexed ts x -> Int
row = getRef . nth nat1 . index

belowBy, aboveBy :: Int -> RefList Rel2
belowBy = dimensional nat2 . Rel
aboveBy = belowBy . negate

below, above :: RefList Rel2
below = belowBy 1
above = aboveBy 1

-- Three dimensions...

type Rel3 = Relative :-: Rel2
type Nat3 = Succ Nat2

nat3 :: Natural Nat3
nat3 = reifyNatural

type Sheet3  = Nested  (NestedNTimes Nat3 Tape)
type ISheet3 = Indexed (NestedNTimes Nat3 Tape)

here3 :: RefList Rel3
here3 = Rel 0 :-: here2

d3 :: (CombineRefLists Rel3 x) => RefList x -> RefList (Rel3 & x)
d3 = (here3 &)

levelAt :: Int -> RefList (Tack Absolute Rel2)
levelAt = dimensional nat3 . Abs

level :: (Nat2 < NestedCount ts) => Indexed ts x -> Int
level = getRef . nth nat2 . index

outwardBy, inwardBy :: Int -> RefList Rel3
outwardBy = dimensional nat3 . Rel
inwardBy  = outwardBy . negate

outward, inward :: RefList Rel3
outward = outwardBy 1
inward  = inwardBy  1

-- Four dimensions...

type Rel4 = Relative :-: Rel3
type Nat4 = Succ Nat3

nat4 :: Natural Nat4
nat4 = reifyNatural

type Sheet4  = Nested  (NestedNTimes Nat4 Tape)
type ISheet4 = Indexed (NestedNTimes Nat4 Tape)

here4 :: RefList Rel4
here4 = Rel 0 :-: here3

d4 :: (CombineRefLists Rel4 x) => RefList x -> RefList (Rel4 & x)
d4 = (here4 &)

spaceAt :: Int -> RefList (Tack Absolute Rel3)
spaceAt = dimensional nat4 . Abs

space :: (Nat3 < NestedCount ts) => Indexed ts x -> Int
space = getRef . nth nat3 . index

anaBy, kataBy :: Int -> RefList Rel4
anaBy  = dimensional nat4 . Rel
kataBy = anaBy . negate

ana, kata :: RefList Rel4
ana  = anaBy  1
kata = kataBy 1
