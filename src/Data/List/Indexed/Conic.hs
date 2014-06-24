{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-} 

module Data.List.Indexed.Conic where

import Data.Numeric.Witness.Peano

-- Tagged lists are lists where each element is an (f a) for some a, but the a may be different for each element. Types of elements are kept track of in the type of the list.

infixr 5 :-:
data x :-: y
data Nil

data ConicList f ts where
   (:-:) :: f a -> ConicList f rest -> ConicList f (a :-: rest)
   ConicNil  :: ConicList f Nil

type family Replicate n x where
   Replicate Zero     x = Nil
   Replicate (Succ n) x = x :-: Replicate n x

type family Length ts where
   Length Nil        = Zero
   Length (x :-: xs) = Succ (Length xs)

type family Tack x xs where
   Tack a Nil        = a :-: Nil
   Tack a (x :-: xs) = x :-: Tack a xs

tack :: f t -> ConicList f ts -> ConicList f (Tack t ts)
tack a ConicNil   = a :-: ConicNil
tack a (x :-: xs) = x :-: tack a xs
