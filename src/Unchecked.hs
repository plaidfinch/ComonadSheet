{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds  #-}

module Unchecked where

import Generic
import Composition
import Indexed

import Data.Function
import Control.Comonad
import Control.Applicative
import Data.Traversable

evaluate :: (ComonadApply w) => w (w a -> a) -> w a
evaluate fs = fix $ (fs <@>) . duplicate

cell :: (Comonad w, Go r w) => r -> w a -> a
cell = (extract .) . go

cells :: (Traversable t, Comonad w, Go r w) => t r -> w a -> t a
cells = traverse cell

sheet :: (Insertable c l t (t a -> a), Applicative t) => a -> l -> t (t a -> a)
sheet background functions = insert functions (pure (const background))

indexedSheet :: (Insertable c l t (t a -> a), Applicative t) => i -> a -> l -> Indexed i t (t a -> a)
indexedSheet i = (Indexed i .) . sheet
