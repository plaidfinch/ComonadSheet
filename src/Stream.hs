module Stream
   ( fold
   , module Data.Stream
   ) where

import Data.Stream
import Control.Comonad
import Data.Distributive
import Data.Traversable
import Data.Foldable hiding ( fold )

import Control.Arrow
import Control.Applicative
import Data.Monoid

import Prelude hiding ( iterate , tail , head )

instance Comonad Stream where
   extract   = head
   duplicate = iterate tail

instance ComonadApply Stream where (<@>) = (<*>)

instance Distributive Stream where
   distribute = unfold (fmap head &&& fmap tail)

fold :: (a -> b -> b) -> Stream a -> b
fold f (Cons x xs) = f x $ fold f xs

instance Foldable Stream where
   foldMap f = fold mappend . fmap f

instance Traversable Stream where
   sequenceA = fold (liftA2 Cons)
