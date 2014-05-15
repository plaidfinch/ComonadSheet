{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ConstraintKinds       #-}

module CountedList where

import Peano

import Control.Applicative
import Data.List hiding ( replicate )

import Prelude hiding ( replicate )

infixr 5 :::

data CountedList n a where
   (:::) :: a -> CountedList n a -> CountedList (Succ n) a
   CNil  :: CountedList Zero a

instance (Show x) => Show (CountedList n x) where
   showsPrec p xs = showParen (p > 10) $
      (showString $ ( intercalate " ::: "
                    $ map show
                    $ unCount xs ) ++ " ::: CNil")

count :: CountedList n a -> Natural n
count CNil       = Zero
count (x ::: xs) = Succ (count xs)

unCount :: CountedList n a -> [a]
unCount CNil       = []
unCount (x ::: xs) = x : unCount xs

replicate :: Natural n -> x -> CountedList n x
replicate Zero _ = CNil
replicate (Succ n) x = x ::: replicate n x

instance Functor (CountedList n) where
   fmap f CNil       = CNil
   fmap f (a ::: as) = f a ::: fmap f as

instance Applicative (CountedList Zero) where
   pure _  = CNil
   _ <*> _ = CNil

instance (Applicative (CountedList n)) => Applicative (CountedList (Succ n)) where
   pure x = x ::: pure x
   (a ::: as) <*> (b ::: bs) = (a b) ::: (as <*> bs)

type family PadNatTo n m where
   PadNatTo Zero     Zero     = Zero
   PadNatTo (Succ n) Zero     = Succ n
   PadNatTo (Succ n) (Succ m) = Succ (PadNatTo n m)

class Pad n m where
   padTo :: Natural n -> x -> CountedList m x -> CountedList (PadNatTo n m) x
instance Pad Zero Zero where
   padTo _ _ _ = CNil
instance (Applicative (CountedList n)) => Pad (Succ n) Zero where
   padTo n x CNil = pure x
instance (Applicative (CountedList n), Pad n m) => Pad (Succ n) (Succ m) where
   padTo (Succ n) x (y ::: ys) = y ::: padTo n x ys

type Paddable n m = ( Pad n m , n ~ PadNatTo n m )
