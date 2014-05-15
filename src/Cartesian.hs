{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Cartesian where

import Control.Applicative

import Tape
import CountedList
import Nested
import Peano

class Cross n t where
   cross :: CountedList n (t a) -> Nested (NestedNTimes n t) (CountedList n a)

instance (Functor t) => Cross (Succ Zero) t where
   cross (t ::: _) =
      Flat $ (::: CNil) <$> t

instance ( Cross (Succ n) t , Functor t
         , Functor (Nested (NestedNTimes (Succ n) t)) )
         => Cross (Succ (Succ n)) t where
   cross (t ::: ts) =
      Nest $ (\xs -> (::: xs) <$> t) <$> cross ts

