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

type family Cartesian f where
   Cartesian (f a :*: as) = CartesianIter f as a
   Cartesian (f a)        = f a

type family CartesianIter f as bs where
   CartesianIter f (t a :*: as) bs = CartesianIter (Compose f t) as (a :*: bs)
   CartesianIter f (t a)        bs = Compose f t (a :*: bs)

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
