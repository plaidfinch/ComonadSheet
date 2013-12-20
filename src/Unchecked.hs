module Unchecked where

import Generic
import Z1
import Z2

import Control.Arrow (first,second)
import Data.Function

evaluate :: (Applicative f, Comonad f) => f (f b -> b) -> f b
evaluate fs = fix $ (fs <*>) . duplicate
--evaluate = extend wfix -- more elegant, but breaks sharing, resulting in exponential performance penalty

cell :: (RefOf r z, AnyZipper z i a) => r -> z -> a
cell = (view .) . go

cells :: (RefOf r z, AnyZipper z i a) => [r] -> z -> [a]
cells refs zipper = map (flip cell zipper) refs

genericSheet :: (Ord r, Enum r, Ord c, Enum c) =>
                ([Z1 c d] -> Z1 r (Z1 c d) -> Z1 r (Z1 c d))
             -> ([d]      -> Z1 c d        -> Z1 c d)
             -> d -> (a -> d) -> [[a]] -> Z2 c r d
genericSheet colInsert rowInsert def inject =
   Z2 .  flip colInsert (fromZ2 (pure def)) .
   fmap (flip rowInsert (pure def)) .
   (fmap . fmap $ inject)

sheetOf :: (Ord r, Enum r, Ord c, Enum c) => a -> [[Z2 c r a -> a]] -> Z2 c r (Z2 c r a -> a)
sheetOf def = genericSheet insertListR insertListR (const def) id
