{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE PolyKinds             #-}

module TaggedList where

import Peano

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
