{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-} 

module IndexedList where

import Peano

import Control.Applicative
import Data.List hiding ( replicate , zip )
import Prelude   hiding ( replicate , zip )

-- Counted lists: indexed by length in the type.

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

append :: CountedList n a -> CountedList m a -> CountedList (m + n) a
append CNil       ys = ys
append (x ::: xs) ys = x ::: append xs ys

nth :: (n < m) => Natural n -> CountedList m a -> a
nth Zero     (a ::: _)  = a
nth (Succ n) (_ ::: as) = nth n as
nth _ _ = error "nth: the impossible occurred" -- like in minus, GHC can't prove this is unreachable

padTo :: (m <= n) => Natural n -> x -> CountedList m x -> CountedList ((n - m) + m) x
padTo n x list =
   list `append` replicate (n `minus` count list) x

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

-- Tagged lists are lists where each element is an (f a) for some a, but the a may be different for each element. Types of elements are kept track of in the type of the list.

infixr 5 :-:
data x :-: y
data Nil

data TaggedList f ts where
   (:-:) :: f a -> TaggedList f rest -> TaggedList f (a :-: rest)
   TNil  :: TaggedList f Nil

type family Replicate n x where
   Replicate Zero     x = Nil
   Replicate (Succ n) x = x :-: Replicate n x

type family Length ts where
   Length Nil        = Zero
   Length (x :-: xs) = Succ (Length xs)

type family Tack x xs where
   Tack a Nil        = a :-: Nil
   Tack a (x :-: xs) = x :-: Tack a xs

tack :: f t -> TaggedList f ts -> TaggedList f (Tack t ts)
tack a TNil       = a :-: TNil
tack a (x :-: xs) = x :-: tack a xs
