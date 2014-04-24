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
   type ListFrom t a
   take :: CountFor t -> t a -> ListFrom t a

instance Take Tape where
   type CountFor Tape = Int
   type ListFrom Tape a = [a]
   take i t | i > 0 = focus t : S.take     (i - 1) (viewR t)
   take i t | i < 0 = focus t : S.take (abs i - 1) (viewL t)
   take _ _ = []

instance (Take t, Functor t) => Take (Compose t Tape) where
   type CountFor (Compose f Tape) = Int :*: CountFor f
   type ListFrom (Compose f Tape) a = ListFrom f [a]
   take (i :*: is) = take is . fmap (take i) . getCompose

instance (Take t) => Take (Indexed i t) where
  type CountFor (Indexed i t) = CountFor t
  type ListFrom (Indexed i t) a = ListFrom t a
  take i (Indexed _ t) = take i t

class (Take t) => Window t where
   window :: CountFor t -> CountFor t -> t a -> ListFrom t a

instance Window Tape where
   window i j t =
      reverse (take (- (abs i)) t)
      ++ tail (take    (abs j)  t)

instance (Window t, Functor t) => Window (Compose t Tape) where
   window (i :*: is) (j :*: js) =
      window is js . fmap (window i j) . getCompose

instance (Window t) => Window (Indexed i t) where
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
