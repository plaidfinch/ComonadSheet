{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module NumericInstances where

import Control.Applicative

instance (Applicative f, Num a) => Num (f a) where
   (+)  = liftA2 (+)
   (*)  = liftA2 (*)
   (-)  = liftA2 (-)
   abs    = fmap abs
   signum = fmap signum
   fromInteger = pure . fromInteger

instance (Enum a, Applicative f) => Enum (f a) where
   succ           = fmap succ
   pred           = fmap pred
   toEnum         = pure . toEnum
   fromEnum       = error "fromEnum: can't go from arbitrary functor to integer"

instance (Ord (f a), Applicative f, Real a) => Real (f a) where
   toRational     = error "toRational: can't go from arbitrary functor to rational"

instance (Fractional a, Applicative f) => Fractional (f a) where
   recip        = fmap recip
   fromRational = pure . fromRational

instance (Enum (f a), Real (f a), Applicative f, Integral a) => Integral (f a) where
   quot          = liftA2 quot
   rem           = liftA2 rem
   div           = liftA2 div
   mod           = liftA2 mod
   x `quotRem` y = (x `quot` y, x `rem` y)
   x `divMod`  y = (x `div`  y, x `mod` y)
   toInteger     = error "toInteger: can't go from arbitrary functor to integer"
