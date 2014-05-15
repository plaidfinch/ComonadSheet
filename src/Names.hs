{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}

module Names where

import Reference
import Peano
import Tape
import Indexed
import Nested
import TaggedList
import CountedList

columnAt :: Int -> RefList (Absolute :-: Nil)
columnAt = dimensional (Succ Zero) . Abs

rightBy, leftBy :: Int -> RefList (Relative :-: Nil)
rightBy = dimensional (Succ Zero) . Rel
leftBy = rightBy . negate

right, left :: RefList (Relative :-: Nil)
right = rightBy 1
left  = leftBy  1

rowAt :: Int -> RefList (Relative :-: Absolute :-: Nil)
rowAt = dimensional (Succ (Succ Zero)) . Abs

belowBy, aboveBy :: Int -> RefList (Relative :-: Relative :-: Nil)
belowBy = dimensional (Succ (Succ Zero)) . Rel
aboveBy = belowBy . negate

below, above :: RefList (Relative :-: Relative :-: Nil)
below = belowBy 1
above = aboveBy 1

levelAt :: Int -> RefList (Relative :-: Relative :-: Absolute :-: Nil)
levelAt = dimensional (Succ (Succ (Succ Zero))) . Abs

inwardBy, outwardBy :: Int -> RefList (Relative :-: Relative :-: Relative :-: Nil)
inwardBy  = dimensional (Succ (Succ (Succ Zero))) . Rel
outwardBy = inwardBy . negate

inward, outward :: RefList (Relative :-: Relative :-: Relative :-: Nil)
inward  = inwardBy  1
outward = outwardBy 1

spaceAt :: Int -> RefList (Relative :-: Relative :-: Relative :-: Absolute :-: Nil)
spaceAt = dimensional (Succ (Succ (Succ (Succ Zero)))) . Abs

anaBy, kataBy :: Int -> RefList (Relative :-: Relative :-: Relative :-: Relative :-: Nil)
anaBy  = dimensional (Succ (Succ (Succ (Succ Zero)))) . Rel
kataBy = anaBy . negate

ana, kata :: RefList (Relative :-: Relative :-: Relative :-: Relative :-: Nil)
ana  = anaBy  1
kata = kataBy 1
