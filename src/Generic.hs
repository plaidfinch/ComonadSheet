{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE OverlappingInstances   #-}

module Generic where

import Control.Applicative

import Stream ( Stream(..) )
import qualified Stream as S

import Prelude hiding ( iterate , take )

import Tape
import Indexed
import Peano
import Composition
import Reference
import Names

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
instance (Functor l, Applicative f, InsertCompose l f, InsertCompose m g) 
   => InsertCompose (Compose l m) (Compose f g) where
      insertCompose (Compose lm)  (Compose fg) =
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

type family DimensionCount f where
   DimensionCount (Indexed i t a) = ComposeCount (t a)
   DimensionCount              a  = ComposeCount    a

class CountDimension f where
   countDimension :: f -> DimensionCount f
instance (CountCompose (t a)) => CountDimension (Indexed i t a) where
   countDimension (Indexed i t) = countCompose t
instance (CountCompose (f (g a))) => CountDimension (Compose f g a) where
   countDimension = countCompose
instance (DimensionCount f ~ Zero) => CountDimension f where
   countDimension _ = Zero

asDimensionalAs :: (ComposeN (DimensionCount g) f, CountDimension g)
                => f -> g -> NthCompose (DimensionCount g) f
f `asDimensionalAs` g = composeN (countDimension g) f

-- | This is a synonym for all the things which must be true in order to insert a list-like structure
--   into some n-dimensional tape-like structure.
type Insertable compList list tape a =
   ( CountDimension (tape a)                                -- can we count tape's dimensionality (yes)?
   , ComposeN (DimensionCount (tape a)) list                -- can we Compose the list that # of times?
   , NthCompose (DimensionCount (tape a)) list ~ compList a -- then let (compList a) be result of this,
   , InsertCompose compList tape )                          -- and, can we insert that into the tape?

insert :: (Insertable c l t a) => l -> t a -> t a
insert l t = insertCompose (l `asDimensionalAs` t) t

dimensionality :: (CountDimension t, WholeFromNat (S (DimensionCount t)))
               => t -> NatToWhole (S (DimensionCount t))
dimensionality = wholeFromNat . S . countDimension

fpow :: Int -> (a -> a) -> a -> a
fpow n = foldr (.) id . replicate n

class Go r t where
   go :: r -> t a -> t a

instance Go Rel Tape where
   go (Rel r) | r > 0      = fpow (abs r) moveR
   go (Rel r) | otherwise  = fpow (abs r) moveL

instance (Functor t) => Go Rel (Compose t Tape) where
   go r = composedly (fmap (go r))

instance (Go r Tape, Go rs t, Functor t) => Go (r :*: rs) (Compose t Tape) where
   go (r :*: rs) = composedly (go rs . fmap (go r))

instance (i ~ (i & DiffOf r i), Combine i (DiffOf r i), Diff r i, Go (DiffOf r i) t) => Go r (Indexed i t)
   where go r (Indexed i t) =
            let move = r `diff` i
            in  Indexed (i & move) (go move t)
