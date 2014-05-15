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

columnAt :: Int -> TaggedList Ref (Absolute :-: Nil)
columnAt = dimensional (Succ Zero) . Abs

rightBy, leftBy :: Int -> TaggedList Ref (Relative :-: Nil)
rightBy = dimensional (Succ Zero) . Rel
leftBy = rightBy . negate

right, left :: TaggedList Ref (Relative :-: Nil)
right = rightBy 1
left  = leftBy  1

rowAt :: Int -> TaggedList Ref (Relative :-: Absolute :-: Nil)
rowAt = dimensional (Succ (Succ Zero)) . Abs

belowBy, aboveBy :: Int -> TaggedList Ref (Relative :-: Relative :-: Nil)
belowBy = dimensional (Succ (Succ Zero)) . Rel
aboveBy = belowBy . negate

below, above :: TaggedList Ref (Relative :-: Relative :-: Nil)
below = belowBy 1
above = aboveBy 1

levelAt :: Int -> TaggedList Ref (Relative :-: Relative :-: Absolute :-: Nil)
levelAt = dimensional (Succ (Succ (Succ Zero))) . Abs

inwardBy, outwardBy :: Int -> TaggedList Ref (Relative :-: Relative :-: Relative :-: Nil)
inwardBy  = dimensional (Succ (Succ (Succ Zero))) . Rel
outwardBy = inwardBy . negate

inward, outward :: TaggedList Ref (Relative :-: Relative :-: Relative :-: Nil)
inward  = inwardBy  1
outward = outwardBy 1

spaceAt :: Int -> TaggedList Ref (Relative :-: Relative :-: Relative :-: Absolute :-: Nil)
spaceAt = dimensional (Succ (Succ (Succ (Succ Zero)))) . Abs

anaBy, kataBy :: Int -> TaggedList Ref (Relative :-: Relative :-: Relative :-: Relative :-: Nil)
anaBy  = dimensional (Succ (Succ (Succ (Succ Zero)))) . Rel
kataBy = anaBy . negate

ana, kata :: TaggedList Ref (Relative :-: Relative :-: Relative :-: Relative :-: Nil)
ana  = anaBy  1
kata = kataBy 1
