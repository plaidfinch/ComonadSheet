--{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}

module ComonadComposition where

import Control.Comonad
import Control.Arrow
import Control.Applicative
import Data.Distributive
import Data.Functor.Compose
import Data.Stream
import Prelude hiding ( map , tail , iterate , take , head )

data Z a = Z (Stream a) a (Stream a)

zipL, zipR :: Z a -> Z a
zipR (Z ls c (Cons r rs)) = Z (Cons c ls) r rs
zipL (Z (Cons l ls) c rs) = Z ls l (Cons c rs)

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

instance Functor Z where
   fmap f (Z ls c rs) = Z (map f ls) (f c) (map f rs)

instance Comonad Stream where
   extract (Cons a _) = a
   duplicate = iterate tail

instance Comonad Z where
   extract (Z _ c _) = c
   duplicate = zipIterate zipL zipR

instance Distributive Stream where
   distribute = unfold (fmap extract &&& fmap tail)

instance Distributive Z where
   distribute =
      zipUnfold (fmap extract &&& fmap zipL)
                (fmap extract)
                (fmap extract &&& fmap zipR)

composedly :: (f (g a) -> f' (g' a')) -> Compose f g a -> Compose f' g' a'
composedly f = Compose . f . getCompose

composedly2 f = Compose . Compose . f . getCompose . getCompose

instance (Comonad f, Comonad g, Distributive f, Distributive g) => Comonad (Compose f g) where
   extract   = extract . extract . getCompose
   duplicate = Compose . (fmap . fmap) Compose . collect duplicate . collect duplicate . getCompose
