{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE OverlappingInstances  #-}

module Cartesian where

import Data.Functor.Compose
import Control.Applicative

import Tape
import Reference

type family Cartesian ts where
   Cartesian (t a :*: ts) = CartesianIter ts t a
   Cartesian (t a)        = t a

type family CartesianIter ts t as where
   CartesianIter (t a :*: ts) f as = CartesianIter ts (Compose f t) (a :*: as)
   CartesianIter (t a)        f as = Compose f t (a :*: as)

class Cross a where
   cross :: a -> Cartesian a
instance Cross (Tape a) where
   cross = id
instance ( Cartesian (t a :*: ts) ~ Compose s t (a :*: as) , Cartesian ts ~ s as
         , Cross ts, Applicative s, Applicative t ) => Cross (t a :*: ts) where
   cross (t :*: ts) = cross2 t (cross ts)

-- | Cartesian product space for two Tapes.
cross2 :: (Applicative t, Applicative s) => t a -> s b -> Compose s t (a :*: b)
cross2 a b = (:*:) <$> Compose (     pure a)
                   <*> Compose (fmap pure b)
