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
   CartesianIter f (g a :*: as) bs = CartesianIter (Compose f g) as (a :*: bs)
   CartesianIter f (t a) bs = Compose f t (a :*: bs)

class Cross a where
   cross :: a -> Cartesian a
instance Cross (Tape a) where
   cross = id
instance (CartesianIter t as a ~ Compose s t (a :*: bs), Cartesian as ~ s bs, Cross as, Applicative s, Applicative t) => Cross (t a :*: as) where
   cross (a :*: as) = cross2 a (cross as)

-- | Cartesian product space for two Tapes.
cross2 :: (Applicative t, Applicative t') => t a -> t' b -> Compose t' t (a :*: b)
cross2 a b = (:*:) <$> Compose (     pure a)
                   <*> Compose (fmap pure b)
