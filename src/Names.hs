{-# LANGUAGE TypeOperators #-}

module Names where

import Reference
import Peano
import Tape
import Indexed
import Composition

type Tape2 = Compose Tape  Tape
type Tape3 = Compose Tape2 Tape
type Tape4 = Compose Tape3 Tape

type ITape  c       = Indexed (Abs c)                               Tape
type ITape2 c r     = Indexed (Abs c :*: Abs r)                     Tape2
type ITape3 c r l   = Indexed (Abs c :*: Abs r :*: Abs l)           Tape3
type ITape4 c r l s = Indexed (Abs c :*: Abs r :*: Abs l :*: Abs s) Tape4

columnAt :: a -> Abs a
columnAt = dimensional One . Abs

rightBy, leftBy :: Int -> Rel
rightBy = dimensional One . Rel
leftBy = rightBy . negate

right, left :: Rel
right = rightBy 1
left  = leftBy  1

rowAt :: a -> Rel :*: Abs a
rowAt = dimensional (S One) . Abs

belowBy, aboveBy :: Int -> Rel :*: Rel
belowBy = dimensional (S One) . Rel
aboveBy = belowBy . negate

below, above :: Rel :*: Rel
below = belowBy 1
above = aboveBy 1

levelAt :: a -> Rel :*: Rel :*: Abs a
levelAt = dimensional (S (S One)) . Abs

inwardBy, outwardBy :: Int -> Rel :*: Rel :*: Rel
inwardBy  = dimensional (S (S One)) . Rel
outwardBy = inwardBy . negate

inward, outward :: Rel :*: Rel :*: Rel
inward  = inwardBy  1
outward = outwardBy 1

spaceAt :: a -> Rel :*: Rel :*: Rel :*: Abs a
spaceAt = dimensional (S (S (S One))) . Abs

anaBy, kataBy :: Int -> Rel :*: Rel :*: Rel :*: Rel
anaBy  = dimensional (S (S (S One))) . Rel
kataBy = anaBy . negate

ana, kata :: Rel :*: Rel :*: Rel :*: Rel
ana  = anaBy  1
kata = kataBy 1
