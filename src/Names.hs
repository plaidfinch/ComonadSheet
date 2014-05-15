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

type Tape1 = Nested (NestedNTimes (Succ Zero) Tape)
type ITape1 = Indexed (NestedNTimes (Succ Zero) Tape)

columnAt :: Int -> RefList (Absolute :-: Nil)
columnAt = dimensional (Succ Zero) . Abs

column :: (Nth Zero (NestedCount ts)) => Indexed ts x -> Int
column = (\(Abs a) -> a) . nth Zero . index

rightBy, leftBy :: Int -> RefList (Relative :-: Nil)
rightBy = dimensional (Succ Zero) . Rel
leftBy = rightBy . negate

right, left :: RefList (Relative :-: Nil)
right = rightBy 1
left  = leftBy  1

type Tape2 = Nested (NestedNTimes (Succ (Succ Zero)) Tape)
type ITape2 = Indexed (NestedNTimes (Succ (Succ Zero)) Tape)

rowAt :: Int -> RefList (Relative :-: Absolute :-: Nil)
rowAt = dimensional (Succ (Succ Zero)) . Abs

row :: (Nth (Succ Zero) (NestedCount ts)) => Indexed ts x -> Int
row = (\(Abs a) -> a) . nth (Succ Zero) . index

belowBy, aboveBy :: Int -> RefList (Relative :-: Relative :-: Nil)
belowBy = dimensional (Succ (Succ Zero)) . Rel
aboveBy = belowBy . negate

below, above :: RefList (Relative :-: Relative :-: Nil)
below = belowBy 1
above = aboveBy 1

type Tape3 = Nested (NestedNTimes (Succ (Succ (Succ Zero))) Tape)
type ITape3 = Indexed (NestedNTimes (Succ (Succ (Succ Zero))) Tape)

levelAt :: Int -> RefList (Relative :-: Relative :-: Absolute :-: Nil)
levelAt = dimensional (Succ (Succ (Succ Zero))) . Abs

level :: (Nth (Succ (Succ Zero)) (NestedCount ts)) => Indexed ts x -> Int
level = (\(Abs a) -> a) . nth (Succ (Succ Zero)) . index

inwardBy, outwardBy :: Int -> RefList (Relative :-: Relative :-: Relative :-: Nil)
inwardBy  = dimensional (Succ (Succ (Succ Zero))) . Rel
outwardBy = inwardBy . negate

inward, outward :: RefList (Relative :-: Relative :-: Relative :-: Nil)
inward  = inwardBy  1
outward = outwardBy 1

type Tape4 = Nested (NestedNTimes (Succ (Succ (Succ (Succ Zero)))) Tape)
type ITape4 = Indexed (NestedNTimes (Succ (Succ (Succ (Succ Zero)))) Tape)

spaceAt :: Int -> RefList (Relative :-: Relative :-: Relative :-: Absolute :-: Nil)
spaceAt = dimensional (Succ (Succ (Succ (Succ Zero)))) . Abs

space :: (Nth (Succ (Succ (Succ Zero))) (NestedCount ts)) => Indexed ts x -> Int
space = (\(Abs a) -> a) . nth (Succ (Succ (Succ Zero))) . index

anaBy, kataBy :: Int -> RefList (Relative :-: Relative :-: Relative :-: Relative :-: Nil)
anaBy  = dimensional (Succ (Succ (Succ (Succ Zero)))) . Rel
kataBy = anaBy . negate

ana, kata :: RefList (Relative :-: Relative :-: Relative :-: Relative :-: Nil)
ana  = anaBy  1
kata = kataBy 1
