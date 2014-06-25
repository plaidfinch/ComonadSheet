{- |
Module      :  Control.Comonad.Sheet.Manipulate
Description :  Generic functions for manipulating multi-dimensional comonadic spreadsheets.
Copyright   :  Copyright (c) 2014 Kenneth Foner

Maintainer  :  kenneth.foner@gmail.com
Stability   :  experimental
Portability :  non-portable

This module defines the 'take', 'view', 'go', and 'insert' functions generically for any dimensionality of sheet. These
constitute the preferred way of manipulating sheets, providing an interface to: take finite slices ('take'), infinite
slices ('view'), move to locations ('go'), and insert finite or infinite structures ('insert').
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Control.Comonad.Sheet.Manipulate where

import Data.Stream.Tape
import Control.Comonad.Sheet.Indexed
import Data.Numeric.Witness.Peano
import Data.Functor.Nested
import Control.Comonad.Sheet.Reference
import Data.List.Indexed hiding ( replicate )

import Data.Stream ( Stream(..) , (<:>) )
import qualified Data.Stream as S

import Control.Applicative
import Prelude hiding ( take )

class Take r t where
   -- | The type of an n-dimensional list extracted from an n-dimensional sheet. For instance:
   --
   -- > ListFrom Sheet2 a == [[a]]
   type ListFrom t a
   -- | Given a 'RefList' and an n-dimensional sheet, return an n-dimensional list corresponding to taking items from
   --   the space until reaching the (relative or absolute) coordinates specified.
   take :: RefList r -> t a -> ListFrom t a

class View r t where
   -- | The type of an n-dimensional stream extracted from an n-dimensional sheet. For instance:
   --
   -- > StreamFrom Sheet2 a == Stream (Stream a)
   type StreamFrom t a
   -- | Given a 'RefList' and an n-dimensional sheet, return an n-dimensional stream corresponding to the "view" in the
   --   direction specified by the sign of each of the coordinates. The direction implied by an absolute coordinate is
   --   the direction from the current focus to that location.
   view :: RefList r -> t a -> StreamFrom t a

class Go r t where
   -- | Given a 'RefList' and an n-dimensional sheet, move to the location specified by the @RefList@ given.
   go :: RefList r -> t a -> t a

-- | Combination of 'go' and 'take': moves to the location specified by the first argument, then takes the amount
--   specified by the second argument.
slice :: (Take r' t, Go r t) => RefList r -> RefList r' -> t a -> ListFrom t a
slice r r' = take r' . go r

-- | Use this to insert a (possibly nested) list-like structure into a (possibly many-dimensional) sheet.
--   Note that the depth of nesting of the structure being inserted must match the number of dimensions of the sheet
--   into which it is being inserted. Note also that the structure being inserted need not be a @Nested@ type; it
--   need only have enough levels of structure (i.e. number of nested lists) to match the dimensionality of the sheet.
insert :: (DimensionalAs x (t a), InsertNested l t, AsDimensionalAs x (t a) ~ l a) => x -> t a -> t a
insert l t = insertNested (l `asDimensionalAs` t) t

-- | Take (n + 1) things from a 'Tape', either in the rightward or leftward directions, depending on the sign of the
--   reference given. If the reference is @(Rel 0)@, return the empty list.
tapeTake :: Ref Relative -> Tape a -> [a]
tapeTake (Rel r) t | r > 0 = focus t : S.take      r  (viewR t)
tapeTake (Rel r) t | r < 0 = focus t : S.take (abs r) (viewL t)
tapeTake _ _ = []

instance Take Nil (Nested (Flat Tape)) where
   type ListFrom (Nested (Flat Tape)) a = [a]
   take _ _ = []

instance (Take Nil (Nested ts), Functor (Nested ts)) => Take Nil (Nested (Nest ts Tape)) where
   type ListFrom (Nested (Nest ts Tape)) a = ListFrom (Nested ts) [a]
   take _ = take (Rel 0 :-: ConicNil)

instance Take (Relative :-: Nil) (Nested (Flat Tape)) where
   type ListFrom (Nested (Flat Tape)) a = [a]
   take (r :-: _) (Flat t) = tapeTake r t

instance ( Functor (Nested ts), Take rs (Nested ts) )
         => Take (Relative :-: rs) (Nested (Nest ts Tape)) where
   type ListFrom (Nested (Nest ts Tape)) a = ListFrom (Nested ts) [a]
   take (r :-: rs) (Nest t) = take rs . fmap (tapeTake r) $ t

instance ( Take (Replicate (NestedCount ts) Relative) (Nested ts)
         , Length r <= NestedCount ts
         , ((NestedCount ts - Length r) + Length r) ~ NestedCount ts
         ) => Take r (Indexed ts) where
   type ListFrom (Indexed ts) a = ListFrom (Nested ts) a
   take r (Indexed i t) = take (heterogenize id (getMovement r i)) t

-- | Given a relative reference, either return the rightward-pointing stream or the leftward one, depending on the
--   sign of the reference. @(Rel 0)@ defaults to rightward.
tapeView :: Ref Relative -> Tape a -> Stream a
tapeView (Rel r) t | r >= 0    = focus t <:> viewR t
tapeView (Rel r) t | otherwise = focus t <:> viewL t

instance View Nil (Nested (Flat Tape)) where
   type StreamFrom (Nested (Flat Tape)) a = Stream a
   view _ (Flat t) = tapeView (Rel 0) t

instance (View Nil (Nested ts), Functor (Nested ts)) => View Nil (Nested (Nest ts Tape)) where
   type StreamFrom (Nested (Nest ts Tape)) a = StreamFrom (Nested ts) (Stream a)
   view _ = view (Rel 0 :-: ConicNil)

instance View (Relative :-: Nil) (Nested (Flat Tape)) where
   type StreamFrom (Nested (Flat Tape)) a = (Stream a)
   view (r :-: _) (Flat t) = tapeView r t

instance ( Functor (Nested ts), View rs (Nested ts) )
         => View (Relative :-: rs) (Nested (Nest ts Tape)) where
   type StreamFrom (Nested (Nest ts Tape)) a = StreamFrom (Nested ts) (Stream a)
   view (r :-: rs) (Nest t) = view rs . fmap (tapeView r) $ t

instance ( View (Replicate (NestedCount ts) Relative) (Nested ts)
         , Length r <= NestedCount ts
         , ((NestedCount ts - Length r) + Length r) ~ NestedCount ts
         ) => View r (Indexed ts) where
   type StreamFrom (Indexed ts) a = StreamFrom (Nested ts) a
   view r (Indexed i t) = view (heterogenize id (getMovement r i)) t

-- | Given a relative reference, move that much in a 'Tape', either rightward or leftward depending on sign.
tapeGo :: Ref Relative -> Tape a -> Tape a
tapeGo (Rel r) = fpow (abs r) (if r > 0 then moveR else moveL)
   where fpow n = foldr (.) id . replicate n -- iterate a function n times

instance Go (Relative :-: Nil) (Nested (Flat Tape)) where
   go (r :-: _) (Flat t) = Flat $ tapeGo r t

instance Go Nil (Nested ts) where go _ = id

instance (Go rs (Nested ts), Functor (Nested ts)) => Go (Relative :-: rs) (Nested (Nest ts Tape)) where
   go (r :-: rs) (Nest t) = Nest . go rs . fmap (tapeGo r) $ t

instance ( Go (Replicate (NestedCount ts) Relative) (Nested ts)
         , Length r <= NestedCount ts
         , ((NestedCount ts - Length r) + Length r) ~ NestedCount ts
         , ReifyNatural (NestedCount ts) )
         => Go r (Indexed ts) where
   go r (Indexed i t) =
      let move = getMovement r i
      in  Indexed (merge move i) (go (heterogenize id move) t)

-- | A @(Signed f a)@ is an @(f a)@ annotated with a sign: either @Positive@ or @Negative@. This is a useful type for
--   specifying the directionality of insertions into sheets. By wrapping a list or stream in a @Negative@ and then
--   inserting it into a sheet, you insert it in the opposite direction to the usual one: leftward, upward, inward...
data Signed f a = Positive (f a)
                | Negative (f a)
                deriving ( Eq , Ord , Show )

-- | In order to insert an n-dimensional list-like structure @(l a)@ into an n-dimensional @Tape@, it's only necessary
--   to define how to insert a 1-dimensional @(l a)@ into a 1-dimensional @Tape@. Add instances of this class if you
--   want to be able to insert custom types into a sheet.
class InsertBase l where
   insertBase :: l a -> Tape a -> Tape a

-- | Inserting a @Tape@ into another @Tape@ replaces the latter with the former completely.
instance InsertBase Tape where
   insertBase t _ = t

-- | Inserting a @Stream@ into a @Tape@ replaces the focus and right side of the @Tape@ with the contents of the stream.
instance InsertBase Stream where
   insertBase (Cons x xs) (Tape ls _ _) = Tape ls x xs

-- | Inserting a @Signed Stream@ into a @Tape@ either behaves just like inserting a regular @Stream@, or (in the @Negative@ case) inserts the stream to the left.
instance InsertBase (Signed Stream) where
   insertBase (Positive (Cons x xs)) (Tape ls _ _) = Tape ls x xs
   insertBase (Negative (Cons x xs)) (Tape _ _ rs) = Tape xs x rs

-- | Inserting a list into a @Tape@ prepends the contents of the list rightwards in the @Tape@, pushing the old focus
--   element rightward (i.e. the head of the list becomes the new focus).
instance InsertBase [] where
   insertBase [] t = t
   insertBase (x : xs) (Tape ls c rs) =
      Tape ls x (S.prefix xs (Cons c rs))

-- | Inserting a @Signed []@ into a @Tape@ either behaves just like inserting a regular list, or (in the @Negative@ case) inserts the list to the left.
instance InsertBase (Signed []) where
   insertBase (Positive []) t = t
   insertBase (Negative []) t = t
   insertBase (Positive (x : xs)) (Tape ls c rs) =
      Tape ls x (S.prefix xs (Cons c rs))
   insertBase (Negative (x : xs)) (Tape ls c rs) =
      Tape (S.prefix xs (Cons c ls)) x rs

-- | This typeclass is the inductive definition for inserting things into higher-dimensional spaces. To make new types
--   insertable, add instances of 'InsertBase', not @InsertNested@.
class InsertNested l t where
   insertNested :: l a -> t a -> t a

instance (InsertBase l) => InsertNested (Nested (Flat l)) (Nested (Flat Tape)) where
   insertNested (Flat l) (Flat t) = Flat $ insertBase l t

instance ( InsertBase l , InsertNested (Nested ls) (Nested ts)
         , Functor (Nested ls) , Applicative (Nested ts) )
         => InsertNested (Nested (Nest ls l)) (Nested (Nest ts Tape)) where
   insertNested (Nest l) (Nest t) =
      Nest $ insertNested (insertBase <$> l) (pure id) <*> t

instance (InsertNested l (Nested ts)) => InsertNested l (Indexed ts) where
   insertNested l (Indexed i t) = Indexed i (insertNested l t)

-- | @DimensionalAs@ provides a mechanism to "lift" an n-deep nested structure into an explicit @Nested@ type. This
--   is the way in which raw lists-of-lists-of-lists, etc. can be inserted (without manual annotation of nesting depth)
--   into a sheet.
class DimensionalAs x y where
   type AsDimensionalAs x y
   -- | @x `asDimensionalAs` y@ applies the appropriate constructors for 'Nested' to @x@ a number of times equal to
   --   the number of dimensions of @y@. For instance:
   --
   --   > [['x']] `asDimensionalAs` Nest (Flat [['y']]) == Nest (Flat [['x']])
   asDimensionalAs :: x -> y -> x `AsDimensionalAs` y

-- | In the case of a @Nested@ structure, @asDimensionalAs@ defaults to @asNestedAs@.
instance (NestedAs x (Nested ts y), AsDimensionalAs x (Nested ts y) ~ AsNestedAs x (Nested ts y)) => DimensionalAs x (Nested ts y) where
   type x `AsDimensionalAs` (Nested ts a) = x `AsNestedAs` (Nested ts a)
   asDimensionalAs = asNestedAs

-- | @DimensionalAs@ also knows the dimensionality of an 'Indexed' sheet as well as regular @Nested@ structures.
instance (NestedAs x (Nested ts y)) => DimensionalAs x (Indexed ts y) where
   type x `AsDimensionalAs` (Indexed ts a) = x `AsNestedAs` (Nested ts a)
   x `asDimensionalAs` (Indexed i t)       = x `asNestedAs` t
