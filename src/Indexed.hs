{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Indexed where

import Control.Comonad
import Control.Applicative
import Data.Functor.Identity

import Control.Lens ( view , over )
import Control.Lens.Tuple

import Tape
import Cartesian
import Prelude hiding ( iterate )

data Indexed i z a =
  Indexed { index     :: i
          , unindexed :: z a
          } deriving ( Functor )

type ITape  c       = Indexed (Identity c) Tape
type ITape2 c r     = Indexed (c,r)        Tape2
type ITape3 c r l   = Indexed (c,r,l)      Tape3
type ITape4 c r l s = Indexed (c,r,l,s)    Tape4

instance (ComonadApply z, Indexes i z) => Comonad (Indexed i z) where
  extract      = extract . unindexed
  duplicate iz = Indexed (index iz) $
                    Indexed <$> indices (index iz)
                            <@> duplicate (unindexed iz)

instance (ComonadApply z, Indexes i z) => ComonadApply (Indexed i z) where
   (Indexed _ fs) <@> (Indexed i xs) = Indexed i (fs <@> xs)

-- Notice that we don't have any instances of Applicative or Distributive for Indexed.
-- This is because, respectively, there's no sensible way to satisfy Applicative's interchange law,
-- and given an arbitrary functor f, there's no way to lift the index up out of an (f (Indexed i z)),
-- as would be necessary to implement distribute (what would you do if f = Maybe, for instance?).

class Indexes i z where
   indices :: i -> z i

instance (Enum a) => Indexes (Identity a) Tape where
   indices = iterate (fmap pred) (fmap succ)

instance (Enum a, Enum b) => Indexes (a,b) Tape2 where
   indices = cross <$> iterate pred succ . view _1
                   <*> iterate pred succ . view _2

instance (Enum a, Enum b, Enum c) => Indexes (a,b,c) Tape3 where
  indices = cross3 <$> iterate pred succ . view _1
                   <*> iterate pred succ . view _2
                   <*> iterate pred succ . view _3

instance (Enum a, Enum b, Enum c, Enum d) => Indexes (a,b,c,d) Tape4 where
  indices = cross4 <$> iterate pred succ . view _1
                   <*> iterate pred succ . view _2
                   <*> iterate pred succ . view _3
                   <*> iterate pred succ . view _4

instance (Dimension1 z, Enum x, Field1 i i x x) => Dimension1 (Indexed i z) where
   zipL = Indexed <$> over _1 pred . index
                  <*> zipL . unindexed
   zipR = Indexed <$> over _1 succ . index
                  <*> zipR . unindexed

instance (Dimension2 z, Enum x, Field2 i i x x) => Dimension2 (Indexed i z) where
   zipU = Indexed <$> over _2 pred . index
                  <*> zipU . unindexed
   zipD = Indexed <$> over _2 succ . index
                  <*> zipD . unindexed

instance (Dimension3 z, Enum x, Field3 i i x x) => Dimension3 (Indexed i z) where
   zipI = Indexed <$> over _3 pred . index
                  <*> zipI . unindexed
   zipO = Indexed <$> over _3 succ . index
                  <*> zipO . unindexed

instance (Dimension4 z, Enum x, Field4 i i x x) => Dimension4 (Indexed i z) where
   zipA = Indexed <$> over _4 pred . index
                  <*> zipA . unindexed
   zipK = Indexed <$> over _4 succ . index
                  <*> zipK . unindexed
