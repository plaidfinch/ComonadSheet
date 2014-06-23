{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Indexed where

import Control.Comonad
import Control.Applicative
import Data.Functor.Identity
import Data.Functor.Compose

import Peano
import Tape
import Reference
import Nested
import IndexedList

type Coordinate n = CountedList n (Ref Absolute)

data Indexed ts a =
  Indexed { index     :: Coordinate (NestedCount ts)
          , unindexed :: Nested ts a }

instance (Functor (Nested ts)) => Functor (Indexed ts) where
   fmap f (Indexed i t) = Indexed i (fmap f t)

type Indexable ts = ( Cross (NestedCount ts) Tape , ts ~ NestedNTimes (NestedCount ts) Tape )

instance (ComonadApply (Nested ts), Indexable ts) => Comonad (Indexed ts) where
   extract      = extract . unindexed
   duplicate it = Indexed (index it) $
                     Indexed <$> indices (index it)
                             <@> duplicate (unindexed it)

instance (ComonadApply (Nested ts), Indexable ts) => ComonadApply (Indexed ts) where
   (Indexed i fs) <@> (Indexed _ xs) = Indexed i (fs <@> xs)

indices :: (Cross n Tape) => Coordinate n -> Nested (NestedNTimes n Tape) (Coordinate n)
indices = cross . fmap enumerate

class Cross n t where
   cross :: CountedList n (t a) -> Nested (NestedNTimes n t) (CountedList n a)

instance (Functor t) => Cross (Succ Zero) t where
   cross (t ::: _) =
      Flat $ (::: CNil) <$> t

instance ( Cross (Succ n) t , Functor t
         , Functor (Nested (NestedNTimes (Succ n) t)) )
         => Cross (Succ (Succ n)) t where
   cross (t ::: ts) =
      Nest $ (\xs -> (::: xs) <$> t) <$> cross ts

type family NestedCount x where
   NestedCount (Flat f)   = Succ Zero
   NestedCount (Nest f g) = Succ (NestedCount f)

type family NestedNTimes n f where
   NestedNTimes (Succ Zero) f = Flat f
   NestedNTimes (Succ n)    f = Nest (NestedNTimes n f) f
