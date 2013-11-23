{-# LANGUAGE FlexibleInstances #-}

module NumericInstances where

import Control.Applicative

instance (Applicative f, Num a) => Num (f a) where
   a + b  = (+) <$> a <*> b
   a * b  = (*) <$> a <*> b
   a - b  = (-) <$> a <*> b
   abs    = fmap abs
   signum = fmap signum
   fromInteger = pure . fromInteger

