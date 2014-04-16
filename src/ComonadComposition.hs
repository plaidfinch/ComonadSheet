{-# LANGUAGE DeriveFunctor , FlexibleInstances #-}

module ComonadComposition where

import Control.Comonad
import Control.Arrow
import Control.Applicative
import Data.Distributive
import Data.Functor
import Data.Functor.Compose
import Data.Stream

import Prelude hiding ( iterate , tail )

composedly :: (f (g a) -> f' (g' a')) -> Compose f g a -> Compose f' g' a'
composedly f = Compose . f . getCompose

data Z a = Z (Stream a) a (Stream a) deriving ( Functor )

class Zipper1 z where
  zipL :: z a -> z a
  zipR :: z a -> z a

instance Zipper1 Z where
  zipR (Z ls c (Cons r rs)) = Z (Cons c ls) r rs
  zipL (Z (Cons l ls) c rs) = Z ls l (Cons c rs)

instance (Functor f) => Zipper1 (Compose f Z) where
  zipL = composedly (fmap zipL)
  zipR = composedly (fmap zipR)

class Zipper2 z where
  zipU :: z a -> z a
  zipD :: z a -> z a

instance (Zipper1 z) => Zipper2 (Compose z Z) where
  zipU = composedly zipL
  zipD = composedly zipR

class Zipper3 z where
  zipI :: z a -> z a
  zipO :: z a -> z a

instance (Zipper2 z) => Zipper3 (Compose z Z) where
  zipI = composedly zipU
  zipO = composedly zipD

-- Some example zippers for testing...
ints :: Z Int
ints = zipIterate pred succ 0

intPairs :: Compose Z Z (Int,Int)
intPairs = (,) <$> Compose (     pure ints)
               <*> Compose (fmap pure ints)

intTriples :: Compose (Compose Z Z) Z (Int,Int,Int)
intTriples = (,,) <$> (Compose . Compose) ((pure      .      pure) ints)
                  <*> (Compose . Compose) ((pure      . fmap pure) ints)
                  <*> (Compose . Compose) ((fmap pure . fmap pure) ints)

viewL, viewR :: Z a -> Stream a
viewL (Z ls _ _) = ls
viewR (Z _ _ rs) = rs

dup :: a -> (a,a)
dup a = (a,a)

zipIterate :: (a -> a) -> (a -> a) -> a -> Z a
zipIterate prev next =
   zipUnfold (dup . prev) id (dup . next)

zipUnfold :: (c -> (a,c)) -> (c -> a) -> (c -> (a,c)) -> c -> Z a
zipUnfold prev center next =
   Z <$> unfold prev <*> center <*> unfold next

instance Comonad Stream where
   extract (Cons a _) = a
   duplicate = iterate tail

instance Comonad Z where
   extract (Z _ c _) = c
   duplicate = zipIterate zipL zipR

instance Applicative Z where
   pure = Z <$> pure <*> id <*> pure
   (Z ls c rs) <*> (Z ls' c' rs') =
      Z (ls <*> ls') (c c') (rs <*> rs')

instance Distributive Stream where
   distribute =
      unfold (fmap extract &&& fmap tail)

instance Distributive Z where
   distribute =
      zipUnfold (fmap extract &&& fmap zipL)
                (fmap extract)
                (fmap extract &&& fmap zipR)

instance ComonadApply Stream where (<@>) = (<*>)
instance ComonadApply Z      where (<@>) = (<*>)

instance (Comonad f, Comonad g, Distributive g) => Comonad (Compose f g) where
   extract   = extract . extract . getCompose
   duplicate = fmap Compose . Compose -- wrap it again: f (g (f (g a))) -> Compose f g (Compose f g a)
             . fmap distribute        -- swap middle two layers: f (f (g (g a))) -> f (g (f (g a)))
             . duplicate              -- duplicate outer functor f: f (g (g a)) -> f (f (g (g a)))
             . fmap duplicate         -- duplicate inner functor g: f (g a) -> f (g (g a))
             . getCompose             -- unwrap it: Compose f g a -> f (g a)
