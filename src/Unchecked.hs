module Unchecked where

import Generic
import Z1
import Z2
import Z3
import Z4

import Control.Arrow (first,second)
import Data.Function

evaluate :: ComonadApply f => f (f b -> b) -> f b
evaluate fs = fix $ (fs <@>) . duplicate
--evaluate = extend wfix -- more elegant, but breaks sharing, resulting in exponential performance penalty

cell :: (RefOf r z l, AnyZipper z i a) => r -> z -> a
cell = (view .) . go

cells :: (RefOf r z l, AnyZipper z i a) => [r] -> z -> [a]
cells refs zipper = map (`cell` zipper) refs

sheet :: (Applicative z, AnyZipper (z a) i a, RefOf ref (z a) list) => i -> a -> list -> z a
sheet origin background list = insert list . reindex origin $ pure background
