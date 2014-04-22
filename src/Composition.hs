{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE OverlappingInstances   #-}

module Composition
   ( ComposeCount(..)
   , CountCompose(..)
   , NthCompose(..)
   , ComposeN(..)
   , composedly
   , asComposedAs
   , module Data.Functor.Compose
   ) where

import Data.Functor.Compose

import Peano
import Indexed

-- | Apply a function to the inside of a @Compose@.
--
--   @composedly f = Compose . f . getCompose@
composedly :: (f (g a) -> f' (g' a')) -> Compose f g a -> Compose f' g' a'
composedly f = Compose . f . getCompose

-- TODO: unmuddle the Indexed thing from ComposeCount, probably by creating a new type family / class such as DimensionCount and using that. This way, we get something nice out of this module which isn't specialized to this particular problem.

type family ComposeCount f where
   ComposeCount (Indexed i t a) = ComposeCount (t a)
   ComposeCount (Compose f g a) = S (ComposeCount (f (g a)))
   ComposeCount x               = Zero

class CountCompose f where
   countCompose :: f -> ComposeCount f
instance (CountCompose (t a)) => CountCompose (Indexed i t a) where
   countCompose (Indexed i t) = countCompose t
instance (CountCompose (f (g a))) => CountCompose (Compose f g a) where
   countCompose (Compose x) = S (countCompose x)
instance (ComposeCount f ~ Zero) => CountCompose f where
   countCompose _ = Zero

type family NthCompose n a where
   NthCompose  Zero       a   = a
   NthCompose (S n) (f (g a)) = NthComposeIter n a (Compose f g)

type family NthComposeIter n a f where
   NthComposeIter  Zero    a  f = f a
   NthComposeIter (S n) (g a) f = NthComposeIter n a (Compose f g)

class ComposeN n f
   where composeN :: n -> f -> NthCompose n f
instance (NthCompose Zero f ~ f) => ComposeN Zero f
   where composeN Zero  = id
instance (ComposeN n f, NthCompose n f ~ g (h a), NthCompose (S n) f ~ Compose g h a) => ComposeN (S n) f
   where composeN (S n) = Compose . composeN n

asComposedAs :: (CountCompose g, ComposeN (ComposeCount g) f) => f -> g -> NthCompose (ComposeCount g) f
f `asComposedAs` g = composeN (countCompose g) f
