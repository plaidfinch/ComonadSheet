{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Insert where

import Stream
import Tape
import Indexed
import Nested

import Control.Applicative

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
      Tape ls x (prefix xs (Cons c rs))

instance InsertBase (Signed []) Tape where
   insertBase (Positive []) t = t
   insertBase (Negative []) t = t
   insertBase (Positive (x : xs)) (Tape ls c rs) =
      Tape ls x (prefix xs (Cons c rs))
   insertBase (Negative (x : xs)) (Tape ls c rs) =
      Tape (prefix xs (Cons c ls)) x rs

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
