{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Reference where

import Peano

newtype Rel   = Rel Int deriving ( Show , Eq , Ord , Enum , Num )
newtype Abs x = Abs x   deriving ( Show , Eq , Ord , Enum , Num , Functor )

infixr 5 :*:
data a :*: b = a :*: b deriving ( Show , Eq , Ord )

type family a & b where
   (a :*: as) & (b :*: bs) = (a & b) :*: (as & bs)
   a          & (b :*: bs) = (a & b) :*: bs
   (a :*: as) &  b         = (a & b) :*: as
   Rel        & Abs b      = Abs b
   Abs a      & Rel        = Abs a
   Rel        & Rel        = Rel

infixr 4 &
class Combine a b where
   (&) :: a -> b -> a & b
instance Combine Rel Rel where
   (Rel a) & (Rel b) = Rel (a + b)
instance (Enum b) => Combine Rel (Abs b) where
   (Rel r) & (Abs b) = Abs $ toEnum (r + fromEnum b)
instance (Enum a) => Combine (Abs a) Rel where
   (Abs a) & (Rel r) = Abs $ toEnum (r + fromEnum a)
instance (Combine Rel b) => Combine Rel (b :*: bs) where
   a & (b :*: bs) = (a & b) :*: bs
instance (Combine a Rel) => Combine (a :*: as) Rel where
   (a :*: as) & b = (a & b) :*: as
instance (Combine (Abs a) b) => Combine (Abs a) (b :*: bs) where
   a & (b :*: bs) = (a & b) :*: bs
instance (Combine a (Abs b)) => Combine (a :*: as) (Abs b) where
   (a :*: as) & b = (a & b) :*: as
instance (Combine a b, Combine as bs) => Combine (a :*: as) (b :*: bs) where
   (a :*: as) & (b :*: bs) = (a & b) :*: (as & bs)

type family DiffOf a b where
   DiffOf (a :*: as) (b :*: bs) = DiffOf a b :*: DiffOf as bs
   DiffOf  a         (b :*: bs) = DiffOf a b
   DiffOf (a :*: as)  b         = DiffOf a b
   DiffOf (Abs a)     Rel       = Rel
   DiffOf  Rel       (Abs a)    = Rel
   DiffOf (Abs a)    (Abs a)    = Rel

class Diff a b where
   diff :: a -> b -> DiffOf a b
instance (Enum a) => Diff (Abs a) (Abs a) where
   diff (Abs a) (Abs b) = Rel (fromEnum a - fromEnum b)
instance Diff (Abs a) Rel where
   diff _ r = r
instance Diff Rel (Abs a) where
   diff r _ = r
instance (Diff a Rel) => Diff (a :*: as) Rel where
   diff (a :*: as) b = diff a b
instance (Diff Rel b) => Diff Rel (b :*: bs) where
   diff a (b :*: bs) = diff a b
instance (Diff a (Abs b)) => Diff (a :*: as) (Abs b) where
   diff (a :*: as) b = diff a b
instance (Diff (Abs a) b) => Diff (Abs a) (b :*: bs) where
   diff a (b :*: bs) = diff a b
instance (Diff a b, Diff as bs) => Diff (a :*: as) (b :*: bs) where
   diff (a :*: as) (b :*: bs) = diff a b :*: diff as bs

type family InDimension n a where
   InDimension One a   = a
   InDimension (S n) a = Rel :*: InDimension n a

class Dimensional n a where
   dimensional :: n -> a -> InDimension n a
instance Dimensional One a where
   dimensional _ a = a
instance (Dimensional n a) => Dimensional (S n) a where
   dimensional (S n) a = Rel 0 :*: dimensional n a

type family Map f a where
   Map f (a :*: b) = (f a :*: Map f b)
   Map f  a        = f a
