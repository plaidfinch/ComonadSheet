{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE UndecidableInstances   #-}

module Generic where

import Control.Applicative

import Stream ( Stream(..) )
import qualified Stream as S

import Prelude hiding ( iterate , take )

import Tape
import Indexed
import Peano
import Nested
import Reference
import TaggedList
import CountedList hiding ( replicate )

data Signed f a = Positive (f a)
                | Negative (f a)
                deriving ( Eq , Ord , Show )

class InsertBase l t where
   insertBase :: l a -> t a -> t a

instance InsertBase Tape Tape where
   insertBase t _ = t

instance InsertBase Stream Tape where
   insertBase (Cons x xs) (Tape ls _ _) = Tape ls x xs

instance InsertBase (Signed Stream) Tape where
   insertBase (Positive (Cons x xs)) (Tape ls _ _) = Tape ls x xs
   insertBase (Negative (Cons x xs)) (Tape _ _ rs) = Tape xs x rs

instance InsertBase [] Tape where
   insertBase [] t = t
   insertBase (x : xs) (Tape ls c rs) =
      Tape ls x (S.prefix xs (Cons c rs))

instance InsertBase (Signed []) Tape where
   insertBase (Positive []) t = t
   insertBase (Negative []) t = t
   insertBase (Positive (x : xs)) (Tape ls c rs) =
      Tape ls x (S.prefix xs (Cons c rs))
   insertBase (Negative (x : xs)) (Tape ls c rs) =
      Tape (S.prefix xs (Cons c ls)) x rs

class InsertNested l t where
   insertNested :: l a -> t a -> t a

instance (InsertBase l t) => InsertNested (Nested (Flat l)) (Nested (Flat t)) where
   insertNested (Flat l) (Flat t) = Flat $ insertBase l t

instance ( InsertBase l t , InsertNested (Nested ls) (Nested ts)
         , Functor (Nested ls) , Applicative (Nested ts) )
         => InsertNested (Nested (Nest ls l)) (Nested (Nest ts t)) where
   insertNested (Nest l) (Nest t) =
      Nest $ insertNested (insertBase <$> l) (pure id) <*> t

instance (InsertNested l (Nested ts)) => InsertNested l (Indexed ts) where
   insertNested l (Indexed i t) = Indexed i (insertNested l t)

type family AsDimensionalAs x y where
   x `AsDimensionalAs` (Indexed ts a) = x `AsNestedAs` (Nested ts a)
   x `AsDimensionalAs` y              = x `AsNestedAs` y

class DimensionalAs x y where
   asDimensionalAs :: x -> y -> x `AsDimensionalAs` y

instance (NestedAs x (Nested ts y), AsDimensionalAs x (Nested ts y) ~ AsNestedAs x (Nested ts y)) => DimensionalAs x (Nested ts y) where
   asDimensionalAs = asNestedAs

instance (NestedAs x (Nested ts y)) => DimensionalAs x (Indexed ts y) where
   x `asDimensionalAs` (Indexed i t) = x `asNestedAs` t

insert :: (DimensionalAs x (t a), InsertNested l t, AsDimensionalAs x (t a) ~ l a) => x -> t a -> t a
insert l t = insertNested (l `asDimensionalAs` t) t

tapeTake :: Ref Relative -> Tape a -> [a]
tapeTake (Rel r) t | r > 0 = focus t : S.take      r  (viewR t)
tapeTake (Rel r) t | r < 0 = focus t : S.take (abs r) (viewL t)
tapeTake _ _ = []

class Take r t where
   type ListFrom t a
   take :: RefList r -> t a -> ListFrom t a

instance Take Nil (Nested (Flat Tape)) where
   type ListFrom (Nested (Flat Tape)) a = [a]
   take _ _ = []

instance (Take Nil (Nested ts), Functor (Nested ts)) => Take Nil (Nested (Nest ts Tape)) where
   type ListFrom (Nested (Nest ts Tape)) a = ListFrom (Nested ts) [a]
   take _ = take (Rel 0 :-: TNil)

instance Take (Relative :-: Nil) (Nested (Flat Tape)) where
   type ListFrom (Nested (Flat Tape)) a = [a]
   take (r :-: _) (Flat t) = tapeTake r t

instance ( Functor (Nested ts), Take rs (Nested ts) )
         => Take (Relative :-: rs) (Nested (Nest ts Tape)) where
   type ListFrom (Nested (Nest ts Tape)) a = ListFrom (Nested ts) [a]
   take (r :-: rs) (Nest t) = take rs . fmap (tapeTake r) $ t

instance ( Take (Replicate (NestedCount ts) Relative) (Nested ts)
         , Paddable (NestedCount ts) (Length r))
         => Take r (Indexed ts) where
   type ListFrom (Indexed ts) a = ListFrom (Nested ts) a
   take r (Indexed i t) = take (heterogenize id (getMovement r i)) t

-- TODO: Add a View class which lets you extract nested streams from tapes.

tapeGo :: Ref Relative -> Tape a -> Tape a
tapeGo (Rel r) = fpow (abs r) (if r > 0 then moveR else moveL)
   where fpow n = foldr (.) id . replicate n -- iterate a function n times

class Go r t where
   go :: RefList r -> t a -> t a

instance Go (Relative :-: Nil) (Nested (Flat Tape)) where
   go (r :-: _) (Flat t) = Flat $ tapeGo r t

instance Go Nil (Nested ts) where go _ = id

instance (Go rs (Nested ts), Functor (Nested ts)) => Go (Relative :-: rs) (Nested (Nest ts Tape)) where
   go (r :-: rs) (Nest t) = Nest . go rs . fmap (tapeGo r) $ t

instance ( Go (Replicate (NestedCount ts) Relative) (Nested ts)
         , Paddable (NestedCount ts) (Length r)
         , Applicative (CountedList (NestedCount ts)))
         => Go r (Indexed ts) where
   go r (Indexed i t) =
      let move = getMovement r i
      in  Indexed (merge move i) (go (heterogenize id move) t)

slice :: (Take r' t, Go r t) => RefList r -> RefList r' -> t a -> ListFrom t a
slice r r' = take r' . go r
