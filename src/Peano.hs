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
