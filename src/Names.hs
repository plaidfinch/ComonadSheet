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

-- Need to redefine these in terms of :*: rather than (,)...
--type ITape  c       = Indexed (Identity c) Tape
--type ITape2 c r     = Indexed (c,r)        Tape2
--type ITape3 c r l   = Indexed (c,r,l)      Tape3
--type ITape4 c r l s = Indexed (c,r,l,s)    Tape4

atCol :: a -> Abs a
atCol = dimensional One . Abs

rightBy, leftBy :: Int -> Rel
rightBy = dimensional One . Rel
leftBy = rightBy . negate

right, left :: Rel
right = rightBy 1
left  = leftBy  1

atRow :: a -> Rel :*: Abs a
atRow = dimensional (S One) . Abs

belowBy, aboveBy :: Int -> Rel :*: Rel
belowBy = dimensional (S One) . Rel
aboveBy = belowBy . negate

below, above :: Rel :*: Rel
below = belowBy 1
above = aboveBy 1
