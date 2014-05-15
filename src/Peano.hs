{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE ScopedTypeVariables    #-}

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
