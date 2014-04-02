module Unchecked where

import Generic

import Data.Function

evaluate :: ComonadApply w => w (w a -> a) -> w a
evaluate fs = fix $ (fs <@>) . duplicate
--evaluate = extend wfix -- more elegant, but breaks sharing, resulting in exponential performance penalty

cell :: (RefOf r z l, AnyZipper z i a) => r -> z -> a
cell = (view .) . go

cells :: (RefOf r z l, AnyZipper z i a) => [r] -> z -> [a]
cells refs zipper = map (`cell` zipper) refs

sheet :: (Applicative z, AnyZipper (z a) i a, RefOf ref (z a) list) => i -> a -> list -> z a
sheet origin background list = insert list . reindex origin $ pure background
