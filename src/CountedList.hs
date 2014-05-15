{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module CountedList where

import Peano

import Control.Applicative
import Data.List hiding ( replicate , zip )

import Prelude hiding ( replicate , zip )

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
replicate Zero     _ = CNil
replicate (Succ n) x = x ::: replicate n x

class (n < m) => Nth n m where
   nth :: Natural n -> CountedList m a -> a
instance Nth Zero (Succ n) where
   nth _ (a ::: _) = a
instance (Nth n m) => Nth (Succ n) (Succ m) where
   nth (Succ n) (_ ::: as) = nth n as

class (m <= n) => Pad n m where
   padTo :: Natural n -> x -> CountedList m x -> CountedList n x
instance Pad Zero Zero where
   padTo _ _ _ = CNil
instance Pad (Succ n) Zero where
   padTo n x CNil = replicate n x
instance (Pad n m) => Pad (Succ n) (Succ m) where
   padTo (Succ n) x (y ::: ys) = y ::: padTo n x ys

zip :: CountedList n a -> CountedList n b -> CountedList n (a,b)
zip (a ::: as) (b ::: bs) = (a,b) ::: zip as bs
zip CNil _ = CNil
zip _ CNil = CNil

instance Functor (CountedList n) where
   fmap f CNil       = CNil
   fmap f (a ::: as) = f a ::: fmap f as

instance (ReifyNatural n) => Applicative (CountedList n) where
   pure x    = replicate (reifyNatural :: Natural n) x
   fs <*> xs = uncurry id <$> zip fs xs
