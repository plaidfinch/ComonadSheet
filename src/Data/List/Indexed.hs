{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE PolyKinds  #-}

module Data.List.Indexed
   ( module Data.List.Indexed.Counted
   , module Data.List.Indexed.Conic
   , heterogenize , homogenize
   ) where

import Data.List.Indexed.Counted
import Data.List.Indexed.Conic

-- We can convert between them using cones and co-cones

heterogenize :: (a -> f t) -> CountedList n a -> ConicList f (Replicate n t)
heterogenize _ CountedNil = ConicNil
heterogenize f (x ::: xs) = f x :-: heterogenize f xs

homogenize :: (forall t. f t -> a) -> ConicList f ts -> CountedList (Length ts) a
homogenize _ ConicNil   = CountedNil
homogenize f (x :-: xs) = f x ::: homogenize f xs
