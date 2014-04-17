{-# LANGUAGE DeriveFunctor , FlexibleInstances , FlexibleContexts , MultiParamTypeClasses , FunctionalDependencies , UndecidableInstances #-}

module ComonadComposition where

import Control.Comonad
import Control.Arrow
import Control.Applicative
import Data.Distributive
import Data.Functor
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Monoid
import Data.Stream

import Control.Lens ( view , over )
import Control.Lens.Tuple

import Prelude hiding ( iterate , tail , head )

-- Missing instances from Data.Stream:

instance Comonad Stream where
   extract   = head
   duplicate = iterate tail

instance ComonadApply Stream where (<@>) = (<*>)

instance Distributive Stream where
   distribute =
      unfold (fmap extract &&& fmap tail)

-- | A Z(ipper) is like a Turing-machine tape: infinite in both directions, with a focus in the middle.
data Z a = Z (Stream a) a (Stream a) deriving ( Functor )

zipUnfold :: (c -> (a,c)) -> (c -> a) -> (c -> (a,c)) -> c -> Z a
zipUnfold prev center next =
   Z <$> unfold prev <*> center <*> unfold next

zipIterate :: (a -> a) -> (a -> a) -> a -> Z a
zipIterate prev next =
   zipUnfold (dup . prev) id (dup . next)
   where dup a = (a,a)

zipEnumerate :: (Enum a) => a -> Z a
zipEnumerate = zipIterate pred succ

-- Instances for various typeclasses to which Z belongs...

instance Comonad Z where
   extract (Z _ c _) = c
   duplicate = zipIterate zipL zipR

instance ComonadApply Z where
   (Z ls c rs) <@> (Z ls' c' rs') =
      Z (ls <@> ls') (c c') (rs <@> rs')

instance Applicative Z where
   (<*>) = (<@>)
   pure  = Z <$> pure <*> id <*> pure

instance Distributive Z where
   distribute =
      zipUnfold (fmap extract &&& fmap zipL)
                (fmap extract)
                (fmap extract &&& fmap zipR)

-- | Apply a function to the inside of a Compose.
composedly :: (f (g a) -> f' (g' a')) -> Compose f g a -> Compose f' g' a'
composedly f = Compose . f . getCompose

-- Now let's get multi-dimensional!

type Z2 = Compose Z  Z
type Z3 = Compose Z2 Z
type Z4 = Compose Z3 Z

class Zipper1 z where
  zipL :: z a -> z a
  zipR :: z a -> z a

class Zipper2 z where
  zipU :: z a -> z a
  zipD :: z a -> z a

class Zipper3 z where
  zipI :: z a -> z a
  zipO :: z a -> z a

class Zipper4 z where
  zipA :: z a -> z a
  zipK :: z a -> z a

instance Zipper1 Z where
  zipR (Z ls c (Cons r rs)) = Z (Cons c ls) r rs
  zipL (Z (Cons l ls) c rs) = Z ls l (Cons c rs)

instance (Functor f) => Zipper1 (Compose f Z) where
  zipL = composedly (fmap zipL)
  zipR = composedly (fmap zipR)

instance (Zipper1 z) => Zipper2 (Compose z Z) where
  zipU = composedly zipL
  zipD = composedly zipR

instance (Zipper2 z) => Zipper3 (Compose z Z) where
  zipI = composedly zipU
  zipO = composedly zipD

instance (Zipper3 z) => Zipper4 (Compose z Z) where
  zipA = composedly zipI
  zipK = composedly zipO

-- This is just like the Applicative instance given for Compose, but it has the additional Distributive constraint because that's necessary for the whole thing to be a comonad.
instance (ComonadApply f, ComonadApply g, Distributive g) => ComonadApply (Compose f g) where
   Compose f <@> Compose x =
      Compose ((<@>) <$> f <@> x)

-- Given a (Compose f g) where f and g are comonads, we can define one sensible Comonad instance for the whole. But! It requires that g be Distributive in addition, which enables us to permute the layerings of comonads to produce the proper result.
instance (Comonad f, Comonad g, Distributive g) => Comonad (Compose f g) where
   extract   = extract . extract . getCompose
   duplicate = fmap Compose . Compose -- wrap it again: f (g (f (g a))) -> Compose f g (Compose f g a)
             . fmap distribute        -- swap middle two layers: f (f (g (g a))) -> f (g (f (g a)))
             . duplicate              -- duplicate outer functor f: f (g (g a)) -> f (f (g (g a)))
             . fmap duplicate         -- duplicate inner functor g: f (g a) -> f (g (g a))
             . getCompose             -- unwrap it: Compose f g a -> f (g a)

-- | Cartesian product space for two Zs.
cross :: Z a -> Z b -> Z2 (a,b)
cross a b = (,) <$> Compose (     pure a)
                <*> Compose (fmap pure b)

-- | Cartesian product space for three Zs.
cross3 :: Z a -> Z b -> Z c -> Z3 (a,b,c)
cross3 a b c = (,,) <$> (Compose . Compose) (     pure .      pure $ a)
                    <*> (Compose . Compose) (     pure . fmap pure $ b)
                    <*> (Compose . Compose) (fmap pure . fmap pure $ c)

-- | Cartesian product space for four Zs.
cross4 :: Z a -> Z b -> Z c -> Z d -> Z4 (a,b,c,d)
cross4 a b c d = (,,,) <$> (Compose . Compose . Compose) (     pure .      pure .      pure $ a)
                       <*> (Compose . Compose . Compose) (     pure .      pure . fmap pure $ b)
                       <*> (Compose . Compose . Compose) (     pure . fmap pure . fmap pure $ c)
                       <*> (Compose . Compose . Compose) (fmap pure . fmap pure . fmap pure $ d)

-- Some example zippers for testing...
ints :: Z Int
ints = zipIterate pred succ 0

-- Indexed zippers stuff starts here...

data Indexed i z a =
  Indexed { index     :: i
          , unindexed :: z a
          } deriving ( Functor )

type IZ  c       = Indexed (Identity c) Z
type IZ2 c r     = Indexed (c,r)        Z2
type IZ3 c r l   = Indexed (c,r,l)      Z3
type IZ4 c r l s = Indexed (c,r,l,s)    Z4

instance (ComonadApply z, Indexes i z) => Comonad (Indexed i z) where
  extract      = extract . unindexed
  duplicate iz = Indexed (index iz) $
                    Indexed <$> indices (index iz)
                            <@> duplicate (unindexed iz)

instance (ComonadApply z, Indexes i z) => ComonadApply (Indexed i z) where
   (Indexed _ fs) <@> (Indexed i xs) = Indexed i (fs <@> xs)

-- Notice that we don't have any instances of Applicative or Distributive for Indexed. This is because there's no sensible way to satisfy the interchange law of Applicative, and given an arbitrary functor f, there's no way to lift the index up out of an (f (Indexed i z)), as would be necessary to implement distribute (what would you do if f = Maybe, for instance?).

class Indexes i z where
   indices :: i -> z i

instance (Enum a) => Indexes (Identity a) Z where
   indices = zipIterate (fmap pred) (fmap succ)

instance (Enum a, Enum b) => Indexes (a,b) Z2 where
   indices = cross <$> zipIterate pred succ . view _1
                   <*> zipIterate pred succ . view _2

instance (Enum a, Enum b, Enum c) => Indexes (a,b,c) Z3 where
  indices = cross3 <$> zipIterate pred succ . view _1
                   <*> zipIterate pred succ . view _2
                   <*> zipIterate pred succ . view _3

instance (Enum a, Enum b, Enum c, Enum d) => Indexes (a,b,c,d) Z4 where
  indices = cross4 <$> zipIterate pred succ . view _1
                   <*> zipIterate pred succ . view _2
                   <*> zipIterate pred succ . view _3
                   <*> zipIterate pred succ . view _4

instance (Zipper1 z, Enum x, Field1 i i x x) => Zipper1 (Indexed i z) where
   zipL = Indexed <$> over _1 pred . index
                  <*> zipL . unindexed
   zipR = Indexed <$> over _1 succ . index
                  <*> zipR . unindexed

instance (Zipper2 z, Enum x, Field2 i i x x) => Zipper2 (Indexed i z) where
   zipU = Indexed <$> over _2 pred . index
                  <*> zipU . unindexed
   zipD = Indexed <$> over _2 succ . index
                  <*> zipD . unindexed

instance (Zipper3 z, Enum x, Field3 i i x x) => Zipper3 (Indexed i z) where
   zipI = Indexed <$> over _3 pred . index
                  <*> zipI . unindexed
   zipO = Indexed <$> over _3 succ . index
                  <*> zipO . unindexed

instance (Zipper4 z, Enum x, Field4 i i x x) => Zipper4 (Indexed i z) where
   zipA = Indexed <$> over _4 pred . index
                  <*> zipA . unindexed
   zipK = Indexed <$> over _4 succ . index
                  <*> zipK . unindexed
