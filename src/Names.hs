{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}

module Names where

import Reference
import Peano
import Tape
import Indexed
import Nested
import TaggedList
import CountedList

-- One dimension...

type Rel1 = Relative :-: Nil
type Nat1 = Succ Zero

nat1 :: Natural Nat1
nat1 = reifyNatural

type Tape1  = Nested  (NestedNTimes Nat1 Tape)
type ITape1 = Indexed (NestedNTimes Nat1 Tape)

here1 :: RefList Rel1
here1 = Rel 0 :-: TNil

d1 :: (CombineRefLists Rel1 x) => RefList x -> RefList (Rel1 & x)
d1 = (here1 &)

columnAt :: Int -> RefList (Absolute :-: Nil)
columnAt = dimensional nat1 . Abs

column :: (Zero < NestedCount ts) => Indexed ts x -> Int
column = (\(Abs a) -> a) . nth Zero . index

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

type Tape2  = Nested  (NestedNTimes Nat2 Tape)
type ITape2 = Indexed (NestedNTimes Nat2 Tape)

here2 :: RefList Rel2
here2 = Rel 0 :-: here1

d2 :: (CombineRefLists Rel2 x) => RefList x -> RefList (Rel2 & x)
d2 = (here2 &)

rowAt :: Int -> RefList (Tack Absolute Rel1)
rowAt = dimensional nat2 . Abs

row :: (Nat1 < NestedCount ts) => Indexed ts x -> Int
row = (\(Abs a) -> a) . nth nat1 . index

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

type Tape3  = Nested  (NestedNTimes Nat3 Tape)
type ITape3 = Indexed (NestedNTimes Nat3 Tape)

here3 :: RefList Rel3
here3 = Rel 0 :-: here2

d3 :: (CombineRefLists Rel3 x) => RefList x -> RefList (Rel3 & x)
d3 = (here3 &)

levelAt :: Int -> RefList (Tack Absolute Rel2)
levelAt = dimensional nat3 . Abs

level :: (Nat2 < NestedCount ts) => Indexed ts x -> Int
level = (\(Abs a) -> a) . nth nat2 . index

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

type Tape4  = Nested  (NestedNTimes Nat4 Tape)
type ITape4 = Indexed (NestedNTimes Nat4 Tape)

here4 :: RefList Rel4
here4 = Rel 0 :-: here3

d4 :: (CombineRefLists Rel4 x) => RefList x -> RefList (Rel4 & x)
d4 = (here4 &)

spaceAt :: Int -> RefList (Tack Absolute Rel3)
spaceAt = dimensional nat4 . Abs

space :: (Nat3 < NestedCount ts) => Indexed ts x -> Int
space = (\(Abs a) -> a) . nth nat3 . index

anaBy, kataBy :: Int -> RefList Rel4
anaBy  = dimensional nat4 . Rel
kataBy = anaBy . negate

ana, kata :: RefList Rel4
ana  = anaBy  1
kata = kataBy 1
