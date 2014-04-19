{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE OverlappingInstances   #-}

module Slice where

import Control.Applicative
import Data.Functor.Compose

import Stream ( Stream(..) )
import qualified Stream as S

import Prelude hiding ( iterate , take )

import Tape
import Indexed

class Take t where
   type CountFor t
   type ListFrom t
   take :: CountFor t -> t -> ListFrom t

instance Take (Tape a) where
   type CountFor (Tape a) = Int
   type ListFrom (Tape a) = [a]
   take i t | i > 0 = focus t : S.take     (i - 1) (viewR t)
   take i t | i < 0 = focus t : S.take (abs i - 1) (viewL t)
   take _ _ = []

instance Take (Tape2 a) where
   type CountFor (Tape2 a) = (Int,Int)
   type ListFrom (Tape2 a) = [[a]]
   take (i,j) = take j . fmap (take i) . getCompose

instance Take (Tape3 a) where
   type CountFor (Tape3 a) = (Int,Int,Int)
   type ListFrom (Tape3 a) = [[[a]]]
   take (i,j,k) = take (j,k) . fmap (take i) . getCompose

instance Take (Tape4 a) where
   type CountFor (Tape4 a) = (Int,Int,Int,Int)
   type ListFrom (Tape4 a) = [[[[a]]]]
   take (i,j,k,l) = take (j,k,l) . fmap (take i) . getCompose

instance (Take (t a)) => Take (Indexed i t a) where
  type CountFor (Indexed i t a) = CountFor (t a)
  type ListFrom (Indexed i t a) = ListFrom (t a)
  take i (Indexed _ t) = take i t

class (Take t) => Window t where
   window :: CountFor t -> CountFor t -> t -> ListFrom t

instance Window (Tape a) where
   window i i' t =
      reverse (take (negate (abs i)) t) ++ tail (take (abs i') t)

instance Window (Tape2 a) where
   window (i,j) (i',j') =
      window i i' . fmap (window j j') . getCompose

instance Window (Tape3 a) where
   window (i,j,k) (i',j',k') =
      window (i,j) (i',j') . fmap (window k k') . getCompose

instance Window (Tape4 a) where
   window (i,j,k,l) (i',j',k',l') =
      window (i,j,k) (i',j',k') . fmap (window l l') . getCompose

instance (Window (t a)) => Window (Indexed i t a) where
  window i i' (Indexed _ t) = window i i' t

data Signed f a = Positive (f a)
                | Negative (f a)
                deriving ( Eq , Ord , Show )

class InsertCompose l t where
   insertCompose :: l a -> t a -> t a

-- | Given the @Compose@ of two list-like things and the @Compose@ of two @Tape@-like things, we can
--   insert the list-like things into the @Tape@-like things if we know how to insert each corresponding
--   level with one another. Thus, other than this instance, all the other instances we need to define
--   are base cases: how to insert a single list-like thing into a single @Tape@.
instance (Functor l, Applicative f, InsertCompose l f, InsertCompose m g) => InsertCompose (Compose l m) (Compose f g)
   where
      insertCompose (Compose lm) (Compose fg) =
         Compose $ insertCompose (fmap insertCompose lm) (pure id) <*> fg

instance (InsertCompose l t) => InsertCompose l (Indexed i t) where
  insertCompose l (Indexed i t) = Indexed i (insertCompose l t)

instance InsertCompose Tape Tape where
   insertCompose t _ = t

instance InsertCompose Stream Tape where
   insertCompose (Cons x xs) (Tape ls _ _) = Tape ls x xs

instance InsertCompose (Signed Stream) Tape where
   insertCompose (Positive (Cons x xs)) (Tape ls _ _) = Tape ls x xs
   insertCompose (Negative (Cons x xs)) (Tape _ _ rs) = Tape xs x rs

instance InsertCompose [] Tape where
   insertCompose [] t = t
   insertCompose (x : xs) (Tape ls c rs) =
      Tape ls x (S.prefix xs (Cons c rs))

instance InsertCompose (Signed []) Tape where
   insertCompose (Positive []) t = t
   insertCompose (Negative []) t = t
   insertCompose (Positive (x : xs)) (Tape ls c rs) =
      Tape ls x (S.prefix xs (Cons c rs))
   insertCompose (Negative (x : xs)) (Tape ls c rs) =
      Tape (S.prefix xs (Cons c ls)) x rs

data S n = S n deriving Show
data Z   = Z   deriving Show

type family CountC f where
   CountC (Compose f g a) = S (CountC (f (g a)))
   CountC x               = Z

class CountCompose f where
   countCompose :: f -> CountC f

instance (CountCompose (f (g a))) => CountCompose (Compose f g a) where
   countCompose = S . countCompose . getCompose

instance (CountC f ~ Z) => CountCompose f where
   countCompose _ = Z

type family ComposeNatIter n a b where
   ComposeNatIter Z     a     f = f a
   ComposeNatIter (S n) (g a) f = ComposeNatIter n a (Compose f g)

type family ComposeNat n a where
   ComposeNat Z a             = a
   ComposeNat (S n) (f (g a)) = ComposeNatIter n a (Compose f g)

class ComposeN n f where
   composeN :: n -> f -> ComposeNat n f

instance (ComposeNat Z f ~ f) => ComposeN Z f where
   composeN _ f = f

instance (ComposeN n f, ComposeNat n f ~ g (h a), ComposeNat (S n) f ~ Compose g h a) => ComposeN (S n) f
   where
   composeN (S n) = Compose . composeN n

asComposedAs :: (ComposeN (CountC g) f, CountCompose g) => f -> g -> ComposeNat (CountC g) f
f `asComposedAs` g = composeN (countCompose g) f

insert :: (InsertCompose cl t, ComposeN (CountC (t a)) l, CountCompose (t a), ComposeNat (CountC (t a)) l ~ cl a) => l -> t a -> t a
insert l t = insertCompose (l `asComposedAs` t) t

class PolyNatToNum n where
   polyNatToNum :: Num a => n -> a
instance PolyNatToNum Z where
   polyNatToNum _ = 0
instance (PolyNatToNum n) => PolyNatToNum (S n) where
   polyNatToNum (S n) = 1 + polyNatToNum n

dimensionality :: (CountCompose t, PolyNatToNum (CountC t)) => Num a => t -> a
dimensionality = (1+) . polyNatToNum . countCompose
