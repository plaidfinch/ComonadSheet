{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Nested where

import Peano

import Control.Applicative
import Control.Comonad
import Data.Foldable
import Data.Traversable
import Data.Distributive

data Flat x
data Nest o i

data Nested fs a where
   Flat :: f a -> Nested (Flat f) a
   Nest :: forall (f :: * -> *) fs a. Nested fs (f a) -> Nested (Nest fs f) a
   -- We need the explicit forall with kind signature to prevent ambiguity stemming from PolyKinds

type family UnNest x where
   UnNest (Nested (Flat f) a)    = f a
   UnNest (Nested (Nest fs f) a) = Nested fs (f a)

unNest :: Nested fs a -> UnNest (Nested fs a)
unNest (Flat x) = x
unNest (Nest x) = x

instance (Functor f) => Functor (Nested (Flat f)) where
   fmap f = Flat . fmap f . unNest

instance (Functor f, Functor (Nested fs)) => Functor (Nested (Nest fs f)) where
   fmap f = Nest . fmap (fmap f) . unNest

instance (Applicative f) => Applicative (Nested (Flat f)) where
   pure    = Flat . pure
   f <*> x = Flat (unNest f <*> unNest x)

instance (Applicative f, Applicative (Nested fs)) => Applicative (Nested (Nest fs f)) where
   pure    = Nest . pure . pure
   f <*> x = Nest ((<*>) <$> unNest f <*> unNest x)

instance (ComonadApply f) => ComonadApply (Nested (Flat f)) where
   f <@> x = Flat (unNest f <@> unNest x)

instance (ComonadApply f, Distributive f, ComonadApply (Nested fs)) => ComonadApply (Nested (Nest fs f)) where
   f <@> x = Nest ((<@>) <$> unNest f <@> unNest x)

instance (Comonad f) => Comonad (Nested (Flat f)) where
   extract   = extract . unNest
   duplicate = fmap Flat . Flat . duplicate . unNest

instance (Comonad f, Comonad (Nested fs), Distributive f, Functor (Nested (Nest fs f))) => Comonad (Nested (Nest fs f)) where
   extract   = extract . extract . unNest
   duplicate =
      fmap Nest . Nest   -- wrap it again: f (g (f (g a))) -> Nested (Nest f g) (Nested (Nest f g) a)
      . fmap distribute  -- swap middle two layers: f (f (g (g a))) -> f (g (f (g a)))
      . duplicate        -- duplicate outer functor f: f (g (g a)) -> f (f (g (g a)))
      . fmap duplicate   -- duplicate inner functor g: f (g a) -> f (g (g a))
      . unNest           -- NOTE: can't pattern-match on constructor or you break laziness!

type family AddNest x where
   AddNest (Nested fs (f x)) = Nested (Nest fs f) x

type family AsNestedAs x y where
   (f x) `AsNestedAs` (Nested (Flat g) b) = Nested (Flat f) x
   x     `AsNestedAs` y                   = AddNest (x `AsNestedAs` (UnNest y))

class NestedAs x y where
   asNestedAs :: x -> y -> x `AsNestedAs` y

instance ( AsNestedAs (f a) (Nested (Flat g) b) ~ Nested (Flat f) a )
         => NestedAs (f a) (Nested (Flat g) b) where
   x `asNestedAs` _ = Flat x

instance ( AsNestedAs (f a) (UnNest (Nested (Nest g h) b)) ~ Nested fs (f' a')
         , AddNest (Nested fs (f' a')) ~ Nested (Nest fs f') a'
         , NestedAs (f a) (UnNest (Nested (Nest g h) b)))
         => NestedAs (f a) (Nested (Nest g h) b) where
   x `asNestedAs` y = Nest (x `asNestedAs` (unNest y))

type family NestedCount x where
   NestedCount (Flat f)   = Succ Zero
   NestedCount (Nest f g) = Succ (NestedCount f)

nestedCount :: Nested f x -> Natural (NestedCount f)
nestedCount (Flat x) = Succ Zero
nestedCount (Nest x) = Succ (nestedCount x)

type family NestedNTimes n f where
   NestedNTimes (Succ Zero) f = Flat f
   NestedNTimes (Succ n)    f = Nest (NestedNTimes n f) f
