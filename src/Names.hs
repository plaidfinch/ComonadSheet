{-# LANGUAGE TypeOperators #-}

module Names where

import Reference
import Peano

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
