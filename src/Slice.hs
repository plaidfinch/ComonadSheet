{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Slice where

import Control.Applicative
import Data.Functor.Compose

import Stream ( Stream(..) )
import qualified Stream as S

import Prelude hiding ( iterate , take )

import Tape

class Take t where
   type Index t
   type List  t
   take :: Index t -> t -> List t

instance Take (Tape a) where
   type Index (Tape a) = Int
   type List  (Tape a) = [a]
   take i t | i > 0 = focus t : S.take     (i - 1) (viewR t)
   take i t | i < 0 = focus t : S.take (abs i - 1) (viewL t)
   take _ _ = []

instance Take (Tape2 a) where
   type Index (Tape2 a) = (Int,Int)
   type List  (Tape2 a) = [[a]]
   take (i,j) = take j . fmap (take i) . getCompose

instance Take (Tape3 a) where
   type Index (Tape3 a) = (Int,Int,Int)
   type List  (Tape3 a) = [[[a]]]
   take (i,j,k) = take (j,k) . fmap (take i) . getCompose

instance Take (Tape4 a) where
   type Index (Tape4 a) = (Int,Int,Int,Int)
   type List  (Tape4 a) = [[[[a]]]]
   take (i,j,k,l) = take (j,k,l) . fmap (take i) . getCompose

class Window t where
   window :: Index t -> Index t -> t -> List t

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

data Signed f a = Positive (f a)
                | Negative (f a)
                deriving ( Eq , Ord , Show )

class InsertC l t where
   insertC :: l a -> t a -> t a

-- | Given the @Compose@ of two list-like things and the @Compose@ of two @Tape@-like things, we can
--   insert the list-like things into the @Tape@-like things if we know how to insert each corresponding
--   level with one another. Thus, other than this instance, all the other instances we need to define
--   are base cases: how to insert a single list-like thing into a single @Tape@.
instance (Functor l, Applicative f, InsertC l f, InsertC m g) => InsertC (Compose l m) (Compose f g)
   where
      insertC (Compose lm) (Compose fg) =
         Compose $ insertC (fmap insertC lm) (pure id) <*> fg

instance InsertC Tape Tape where
   insertC t _ = t

instance InsertC Stream Tape where
   insertC (Cons x xs) (Tape ls _ _) = Tape ls x xs

instance InsertC (Signed Stream) Tape where
   insertC (Positive (Cons x xs)) (Tape ls _ _) = Tape ls x xs
   insertC (Negative (Cons x xs)) (Tape _ _ rs) = Tape xs x rs

instance InsertC [] Tape where
   insertC [] t = t
   insertC (x : xs) (Tape ls c rs) =
      Tape ls x (S.prefix xs (Cons c rs))

instance InsertC (Signed []) Tape where
   insertC (Positive []) t = t
   insertC (Negative []) t = t
   insertC (Positive (x : xs)) (Tape ls c rs) =
      Tape ls x (S.prefix xs (Cons c rs))
   insertC (Negative (x : xs)) (Tape ls c rs) =
      Tape (S.prefix xs (Cons c ls)) x rs

insert1 :: (InsertC f t) => f a -> t a -> t a
insert1 = insertC

insert2 :: (InsertC (Compose f g) t) => f (g a) -> t a -> t a
insert2 = insertC . Compose

insert3 :: (InsertC (Compose (Compose f g) h) t) => f (g (h a)) -> t a -> t a
insert3 = insertC . Compose . Compose

insert4 :: (InsertC (Compose (Compose (Compose f g) h) i) t) => f (g (h (i a))) -> t a -> t a
insert4 = insertC . Compose . Compose . Compose

class Insert l t where
   insert :: l -> t -> t

instance (InsertC f t) => Insert (f a) (t a)
   where insert = insert1

instance (InsertC (Compose f g) t) => Insert (f (g a)) (t a)
   where insert = insert2

instance (InsertC (Compose (Compose f g) h) t) => Insert (f (g (h a))) (t a)
   where insert = insert3

instance (InsertC (Compose (Compose (Compose f g) h) i) t) => Insert (f (g (h (i a)))) (t a)
   where insert = insert4
