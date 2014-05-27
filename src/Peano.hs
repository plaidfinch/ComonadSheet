{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Peano where

data Zero
data Succ n

data Natural n where
   Zero :: Natural Zero
   Succ :: Natural n -> Natural (Succ n)
deriving instance Show (Natural n)

class ReifyNatural n                               where reifyNatural :: Natural n
instance ReifyNatural Zero                         where reifyNatural = Zero
instance (ReifyNatural n) => ReifyNatural (Succ n) where reifyNatural = Succ (reifyNatural :: Natural n)

type family LessThanOrEqual a b where
   LessThanOrEqual Zero Zero         = True
   LessThanOrEqual Zero (Succ m)     = True
   LessThanOrEqual (Succ n) (Succ m) = LessThanOrEqual n m
   LessThanOrEqual x y               = False

type a <= b = (LessThanOrEqual a b ~ True)

type family LessThan a b where
   LessThan Zero Zero         = True
   LessThan Zero (Succ m)     = True
   LessThan (Succ n) (Succ m) = LessThan n m
   LessThan x y               = False

type a < b = (LessThan a b ~ True)

type family x + y where
   x + Zero     = x
   x + (Succ y) = Succ (x + y)

type family x - y where
   x        - Zero     = x
   (Succ x) - (Succ y) = x - y

plus :: Natural a -> Natural b -> Natural (a + b)
plus x Zero     = x
plus x (Succ y) = Succ (plus x y)

minus :: (b <= a) => Natural a -> Natural b -> Natural (a - b)
minus x        Zero     = x
minus (Succ x) (Succ y) = minus x y
minus _ _ = error "minus: the impossible occurred" -- GHC can't prove this is unreachable
