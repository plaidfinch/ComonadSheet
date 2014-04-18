{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

module Slice where

import Control.Applicative
import Data.Functor.Compose
import Stream ( Stream(..) )
import qualified Stream as S
import Prelude hiding ( iterate , take )

import Tape

class Take i t l | t -> i l where
   take :: i -> t -> l

instance Take Int (Tape a) [a] where
   take i t | i > 0 = focus t : S.take     (i - 1) (viewR t)
   take i t | i < 0 = focus t : S.take (abs i - 1) (viewL t)
   take _ _ = []

instance Take (Int,Int) (Tape2 a) [[a]] where
   take (i,j) = take j . fmap (take i) . getCompose

instance Take (Int,Int,Int) (Tape3 a) [[[a]]] where
   take (i,j,k) = take (j,k) . fmap (take i) . getCompose

instance Take (Int,Int,Int,Int) (Tape4 a) [[[[a]]]] where
   take (i,j,k,l) = take (j,k,l) . fmap (take i) . getCompose

class Window i t l | t -> i l where
   window :: i -> i -> t -> l

instance Window Int (Tape a) [a] where
   window i i' t = reverse (take (negate (abs i)) t) ++ tail (take (abs i') t)

instance Window (Int,Int) (Tape2 a) [[a]] where
   window (i,j) (i',j') = window i i' . fmap (window j j') . getCompose

instance Window (Int,Int,Int) (Tape3 a) [[[a]]] where
   window (i,j,k) (i',j',k') = window (i,j) (i',j') . fmap (window k k') . getCompose

instance Window (Int,Int,Int,Int) (Tape4 a) [[[[a]]]] where
   window (i,j,k,l) (i',j',k',l') = window (i,j,k) (i',j',k') . fmap (window l l') . getCompose

data Signed f a = Positive (f a)
                | Negative (f a)
                deriving ( Eq , Ord , Show )

class InsertCompose l t where
   insertCompose :: l a -> t a -> t a

-- | Given the @Compose@ of two list-like things and the @Compose@ of two @Tape@-like things, we can
--   insert the list-like things into the @Tape@-like things if we know how to insert each corresponding
--   level with one another. Thus, other than this instance, all the other instances we need to define
--   are base cases: how to insert a single list-like thing into a single @Tape@.
instance (Functor l, Applicative f, InsertCompose l f, InsertCompose m g) => InsertCompose (Compose l m) (Compose f g) where
   insertCompose (Compose lm) (Compose fg) =
      Compose $ insertCompose (fmap insertCompose lm) (pure id) <*> fg

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

class Insert l t where
   insert :: l -> t -> t

instance (InsertCompose f Tape) => Insert (f a) (Tape a) where
   insert = insertCompose

instance (InsertCompose (Compose f g) Tape2) => Insert (f (g a)) (Tape2 a) where
   insert = insertCompose . Compose

instance (InsertCompose (Compose (Compose f g) h) Tape3) => Insert (f (g (h a))) (Tape3 a) where
   insert = insertCompose . Compose . Compose

instance (InsertCompose (Compose (Compose (Compose f g) h) i) Tape4) => Insert (f (g (h (i a)))) (Tape4 a) where
   insert = insertCompose . Compose . Compose . Compose
