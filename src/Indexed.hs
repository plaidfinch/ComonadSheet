{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Indexed where

import Control.Comonad
import Control.Applicative
import Data.Functor.Identity

import Control.Lens ( view , over )
import Control.Lens.Tuple

import Tape

data Indexed i t a =
  Indexed { index     :: i
          , unindexed :: t a
          } deriving ( Functor )

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

class Indexes i t where
   indices :: i -> t i
