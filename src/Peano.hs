{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeFamilies           #-}

module Peano where

data S n  = S n  deriving ( Show )
data One  = One  deriving ( Show )
data Zero = Zero deriving ( Show )

type family WholeToNat a where
   WholeToNat One   = S Zero
   WholeToNat (S n) = S (WholeToNat n)

class NatFromWhole n where
   natFromWhole :: n -> WholeToNat n
instance NatFromWhole One where
   natFromWhole _ = S Zero
instance (NatFromWhole n) => NatFromWhole (S n) where
   natFromWhole (S n) = S (natFromWhole n)

type family NatToWhole a where
   NatToWhole (S Zero) = One
   NatToWhole (S n)    = S (NatToWhole n)

class WholeFromNat n where
   wholeFromNat :: n -> NatToWhole n
instance WholeFromNat (S Zero) where
   wholeFromNat _ = One
instance (WholeFromNat (S n)) => WholeFromNat (S (S n)) where
   wholeFromNat (S n) = S (wholeFromNat n)

class NatToNum n where
   natToNum :: Num a => n -> a
instance NatToNum Zero where
   natToNum _ = 0
instance NatToNum One where
   natToNum _ = 1
instance (NatToNum n) => NatToNum (S n) where
   natToNum (S n) = 1 + natToNum n
