{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}

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

type family LessThanOrEqual a b where
   LessThanOrEqual Zero Zero         = True
   LessThanOrEqual Zero (Succ m)     = True
   LessThanOrEqual (Succ n) (Succ m) = LessThanOrEqual n m
   LessThanOrEqual x y               = False

type a <= b = (LessThanOrEqual a b ~ True)

class (n <= m) => Nth n m where
   nth :: Natural n -> CountedList m a -> a
instance Nth Zero (Succ n) where
   nth _ (a ::: _) = a
instance (Nth n m) => Nth (Succ n) (Succ m) where
   nth (Succ n) (_ ::: as) = nth n as

class (m <= n) => Pad n m where
   padTo :: Natural n -> x -> CountedList m x -> CountedList n x
instance Pad Zero Zero where
   padTo _ _ _ = CNil
instance (Applicative (CountedList n)) => Pad (Succ n) Zero where
   padTo n x CNil = pure x
instance (Applicative (CountedList n), Pad n m) => Pad (Succ n) (Succ m) where
   padTo (Succ n) x (y ::: ys) = y ::: padTo n x ys

instance Functor (CountedList n) where
   fmap f CNil       = CNil
   fmap f (a ::: as) = f a ::: fmap f as

instance Applicative (CountedList Zero) where
   pure _  = CNil
   _ <*> _ = CNil

instance (Applicative (CountedList n)) => Applicative (CountedList (Succ n)) where
   pure x = x ::: pure x
   (a ::: as) <*> (b ::: bs) = (a b) ::: (as <*> bs)
