{-# LANGUAGE FlexibleInstances , FlexibleContexts , UndecidableInstances , OverlappingInstances , GeneralizedNewtypeDeriving #-}

module NumericInstances where

import Control.Applicative
import Data.String
import Data.Digits
import Data.Char

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

newtype GodelString = GodelString { fromGodelString :: Int } deriving (Eq, Ord, Enum)

instance IsString GodelString where
   fromString s =
      if all (liftA2 (&&) ('A' <=) (<= 'Z')) s
         then GodelString . unDigits 26 .
              map (subtract (pred (fromEnum 'A')) . fromEnum) $ s
         else error "fromString: all characters in string must be in range A-Z"

instance Show GodelString where
   show = map (chr . (+ (pred (fromEnum 'A')))) . digits 26 . fromGodelString
