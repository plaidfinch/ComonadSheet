{- |
Module      :  Control.Comonad.Sheet.Indexed
Description :  Adds absolute position to n-dimensional comonadic spreadsheets.
Copyright   :  Copyright (c) 2014 Kenneth Foner

Maintainer  :  kenneth.foner@gmail.com
Stability   :  experimental
Portability :  non-portable

This module defines the @Indexed@ type, which bolts an absolute coordinate onto a normal nested structure, allowing
you to talk about absolute position as well as relative position.
-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Control.Comonad.Sheet.Indexed where

import Control.Comonad
import Control.Applicative
import Data.Functor.Identity
import Data.Functor.Compose

import Data.Numeric.Witness.Peano
import Data.Stream.Tape
import Control.Comonad.Sheet.Reference
import Data.Functor.Nested
import Data.List.Indexed

-- | An n-dimensional coordinate is a list of length n of absolute references.
type Coordinate n = CountedList n (Ref Absolute)

-- | An indexed sheet is an n-dimensionally nested 'Tape' paired with an n-dimensional coordinate which
--   represents the absolute position of the current focus in the sheet.
data Indexed ts a =
  Indexed { index     :: Coordinate (NestedCount ts)
          , unindexed :: Nested ts a }

instance (Functor (Nested ts)) => Functor (Indexed ts) where
   fmap f (Indexed i t) = Indexed i (fmap f t)

-- | For a sheet to be Indexable, it needs to consist of n-dimensionally nested 'Tape's, such that we can take the
--   cross product of all n tapes to generate a tape of indices.
type Indexable ts = ( Cross (NestedCount ts) Tape , ts ~ NestedNTimes (NestedCount ts) Tape )

instance (ComonadApply (Nested ts), Indexable ts) => Comonad (Indexed ts) where
   extract      = extract . unindexed
   duplicate it = Indexed (index it) $
                     Indexed <$> indices (index it)
                             <@> duplicate (unindexed it)

instance (ComonadApply (Nested ts), Indexable ts) => ComonadApply (Indexed ts) where
   (Indexed i fs) <@> (Indexed _ xs) = Indexed i (fs <@> xs)

-- | Takes an n-coordinate and generates an n-dimensional enumerated space of coordinates.
indices :: (Cross n Tape) => Coordinate n -> Nested (NestedNTimes n Tape) (Coordinate n)
indices = cross . fmap enumerate

-- | The cross product of an n-length counted list of @(t a)@ is an n-nested @t@ of counted lists of @a@.
class Cross n t where
   cross :: CountedList n (t a) -> Nested (NestedNTimes n t) (CountedList n a)

instance (Functor t) => Cross (Succ Zero) t where
   cross (t ::: _) =
      Flat $ (::: CountedNil) <$> t

instance ( Cross (Succ n) t , Functor t
         , Functor (Nested (NestedNTimes (Succ n) t)) )
         => Cross (Succ (Succ n)) t where
   cross (t ::: ts) =
      Nest $ (\xs -> (::: xs) <$> t) <$> cross ts
