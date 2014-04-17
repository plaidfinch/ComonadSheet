{-# LANGUAGE UndecidableInstances , TypeOperators , MultiParamTypeClasses , FunctionalDependencies , FlexibleInstances , DeriveFunctor #-}

module ReferenceExperiments where

import Data.Monoid
import Data.Functor.Identity

-- Now implementing references more type-safely:

newtype Rel   = Rel Int deriving ( Show , Eq , Ord )
newtype Abs x = Abs x   deriving ( Functor , Show , Eq , Ord )

instance Monoid Rel where
   mempty = Rel 0
   (Rel x) `mappend` (Rel y) = Rel (x + y)

infixr 5 &
class (&) r s t | r s -> t where
   (&) :: r -> s -> t

instance (&) Rel Rel Rel where
   (&) = mappend

instance (Enum x) => (&) (Abs x) Rel (Abs x) where
   (Abs x) & (Rel n) = Abs $ toEnum (fromEnum x + n)

instance (Enum x) => (&) Rel (Abs x) (Abs x) where
   (Rel n) & (Abs x) = Abs $ toEnum (fromEnum x + n)

instance ((&) r1 s1 t1, (&) r2 s2 t2) => (&) (r1,r2) (s1,s2) (t1,t2) where
   (r1,r2) & (s1,s2) = (r1 & s1,r2 & s2)

instance ((&) r1 s1 t1, (&) r2 s2 t2, (&) r3 s3 t3) => (&) (r1,r2,r3) (s1,s2,s3) (t1,t2,t3) where
   (r1,r2,r3) & (s1,s2,s3) = (r1 & s1,r2 & s2,r3 & s3)

instance ((&) r1 s1 t1, (&) r2 s2 t2, (&) r3 s3 t3, (&) r4 s4 t4) => (&) (r1,r2,r3,r4) (s1,s2,s3,s4) (t1,t2,t3,t4) where
   (r1,r2,r3,r4) & (s1,s2,s3,s4) = (r1 & s1,r2 & s2,r3 & s3,r4 & s4)

class Deref r v where
   deref :: r -> v -> v

instance (Enum x) => Deref Rel x where
   deref (Rel n) x = toEnum (fromEnum x + n)

instance Deref (Abs x) x where
   deref (Abs x) _ = x

instance (Deref r1 v1, Deref r2 v2) => Deref (r1,r2) (v1,v2) where
   deref (r1,r2) (v1,v2) = (deref r1 v1,deref r2 v2)

instance (Deref r1 v1, Deref r2 v2, Deref r3 v3) => Deref (r1,r2,r3) (v1,v2,v3) where
   deref (r1,r2,r3) (v1,v2,v3) = (deref r1 v1,deref r2 v2,deref r3 v3)

instance (Deref r1 v1, Deref r2 v2, Deref r3 v3, Deref r4 v4) => Deref (r1,r2,r3,r4) (v1,v2,v3,v4) where
   deref (r1,r2,r3,r4) (v1,v2,v3,v4) = (deref r1 v1,deref r2 v2,deref r3 v3,deref r4 v4)

class At r i | i -> r, r -> i where
   at :: i -> r

instance At (Abs i) (Identity i) where
   at = Abs . runIdentity

instance At (Abs i1,Abs i2) (i1,i2) where
   at (i1,i2) = (Abs i1,Abs i2)

instance At (Abs i1,Abs i2,Abs i3) (i1,i2,i3) where
   at (i1,i2,i3) = (Abs i1,Abs i2,Abs i3)

instance At (Abs i1,Abs i2,Abs i3,Abs i4) (i1,i2,i3,i4) where
   at (i1,i2,i3,i4) = (Abs i1,Abs i2,Abs i3,Abs i4)
