{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE UndecidableInstances  #-}

module Indexed where

import Control.Comonad
import Control.Applicative
import Data.Functor.Identity
import Data.Functor.Compose

import Control.Lens ( view , over )
import Control.Lens.Tuple

import Tape
import Reference
import Cartesian

data Indexed i t a =
  Indexed { index     :: i
          , unindexed :: t a
          } deriving ( Functor )

type Indexes i t =                  -- For i to index t...
   ( Cross (Map Tape i)             -- We must be able to take the cartesian product of i's axes
   , TapesFromIndex i               -- We must be able to actually create the tapes for i's axes
   , Cartesian (Map Tape i) ~ t i ) -- The Cartesian product of i's axes must be the same type as (t i)

instance (ComonadApply t, Indexes i t) => Comonad (Indexed i t) where
   extract      = extract . unindexed
   duplicate it = Indexed (index it) $
                     Indexed <$> indices (index it)
                             <@> duplicate (unindexed it)

instance (ComonadApply t, Indexes i t) => ComonadApply (Indexed i t) where
   (Indexed i fs) <@> (Indexed _ xs) = Indexed i (fs <@> xs)

-- Notice that we don't have any instances of Applicative or Distributive for Indexed.
-- This is because, respectively, there's no sensible way to satisfy Applicative's interchange law,
-- and given an arbitrary functor f, there's no way to lift the index up out of an (f (Indexed i t)),
-- as would be necessary to implement distribute (what would you do if f = Maybe, for instance?).

-- TODO: try to define TapesFromIndex in terms of a generic heterogeneous map function

class TapesFromIndex i where
   tapesFromIndex :: i -> Map Tape i
instance (Map Tape a ~ Tape a, Enum a) => TapesFromIndex a where
   tapesFromIndex a = enumerate a
instance (Enum a, TapesFromIndex as) => TapesFromIndex (a :*: as) where
   tapesFromIndex (a :*: as) = enumerate a :*: tapesFromIndex as

indices :: (Cross (Map Tape a), TapesFromIndex a) => a -> Cartesian (Map Tape a)
indices = cross . tapesFromIndex
