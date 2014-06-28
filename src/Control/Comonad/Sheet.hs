{- |
Module      :  Control.Comonad.Sheet
Description :  A library for expressing "spreadsheet-like" computations with absolute and relative references, using fixed-points of n-dimensional comonads.
Copyright   :  Copyright (c) 2014 Kenneth Foner

Maintainer  :  kenneth.foner@gmail.com
Stability   :  experimental
Portability :  non-portable

@ComonadSheet@ is a library for expressing spreadsheet-like computations with absolute and relative references, using fixed-points of n-dimensional comonads. A sheet is an n-dimensionally nested 'Tape', which is a stream infinite in both left and right directions, with a focus element. For instance, @type Sheet1 a = Nested (Flat Tape) a@, which is isomorphic to @Tape a@. Nested @Tape@s describe multi-dimensional grid-like spaces, which I will refer to, rather leadingly, as /sheets/ made up of /cells/.

While a conventional spreadsheet combines the construction and evaluation of a space of formulae into one process for the user, these steps are distinct in the @ComonadSheet@ library. To create a self-referencing spreadsheet-like computation, first construct a multi-dimensional space of functions which take as input a /space of values/ and return a /single value/. Then, take its fixed point using the @evaluate@ function, resulting in a /space of values/. In other words:

> evaluate :: (ComonadApply w) => w (w a -> a) -> w a

For examples of use, see the <https://github.com/kwf/ComonadSheet GitHub page> for the library.

=Creating Sheets

Usually, the best way to create a sheet is using the 'sheet' function, or using the 'pure' method of the 'Applicative' interface. The @sheet@ function takes a default element value, and a structure containing more values, and inserts those values into a space initially filled with the default value. For instance, @sheet 0 [[1]] :: Sheet2 Int@ makes a one- dimensional sheet which is 0 everywhere except the focus, which is 1. Note that because of overloading on @sheet@'s operands, it is usually necessary to give a type signature somewhere. This is generally not a problem because GHC can almost always infer the type you wanted if you give it so much as a top-level signature.

=References and Manipulation

References to sheets are represented as quasi-heterogeneous lists of absolute and relative references. (In the 'Names' module, I've provided names for referring to dimensions up to 4.) A reference which talks about some dimension /n/ can be used to refer to that same relative or absolute location in any sheet of dimension /n/ or greater.

For instance, @rightBy 5@ is a relative reference in the first dimension. If I let @x = sheet 0 [1..] :: Sheet1 Int@, then @extract (go (rightBy 5) x) == 6@. Notice that I used the 'extract' method from the sheet's 'Comonad' instance to pull out the focus element. Another way to express the same thing would be to say @cell (rightBy 5) x@ -- the @cell@ function is the composition of 'extract' and 'go'. In addition to moving around in sheets, I can use references to slice out pieces of them. For instance, @take (rightBy 5) x == [1,2,3,4,5,6]@. (Note that references used in extracting ranges are treated as inclusive.) I can also use a reference to point in a direction and extract an infinite stream (or stream- of-stream-of- streams...) pointed in that direction. For instance, @view right x == [1..]@.

References can be relative or absolute. An absolute reference can only be used to refer to an `Indexed` sheet, as this is the only kind of sheet with a notion of absolute position.

References can be combined using the @(&)@ operator. For example, @columnAt 5 & aboveBy 10@ represents a reference to a location above the current focus position by 10 cells, and at column 5, regardless of the current column position. Relative references may be combined with one another, and absolute and relative references may be combined, but combining two absolute references is a type error.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE TypeFamilies     #-}

module Control.Comonad.Sheet
   ( evaluate
   , cell , cells
   , sheet , indexedSheet
     -- Names for the relevant aspects of some smaller dimensions.
   , module Control.Comonad.Sheet.Names
     -- Generic functions for manipulating multi-dimensional comonadic spreadsheets.
   , module Control.Comonad.Sheet.Manipulate
     -- Adds absolute position to n-dimensional comonadic spreadsheets.
   , module Control.Comonad.Sheet.Indexed
     -- Relative and absolute references to locations in arbitrary-dimensional sheets.
   , module Control.Comonad.Sheet.Reference

     -- The 'Nested' type enables us to abstract over dimensionality. For instance, a 2-dimensional sheet of integers
     -- is represented by a @Nested (Nest (Flat Tape) Tape) Int@.
   , module Data.Functor.Nested
     -- Counted and conic lists, used for representing references.
   , module Data.List.Indexed
     -- The 'Tape' is the base type we use to construct sheets. A 'Tape' is a both-ways-infinite stream, like a Turing
     -- machine's tape.
   , module Data.Stream.Tape
     -- Peano numerals linked to type-level indices.
   , module Data.Numeric.Witness.Peano

     -- Comonads form the basis of sheet evaluation.
   , module Control.Comonad
     -- Distributivity enables composition of comonads.
   , module Data.Distributive
     -- Numeric instances for functions gives us, for functions @f, g :: Num b => a -> b@, e.g.
     -- @f + g == \x -> f x + g x@. This enables concise syntax for specifying numeric cells in sheets.
   , module Data.Numeric.Function
   ) where

import Control.Comonad.Sheet.Names
import Control.Comonad.Sheet.Manipulate
import Control.Comonad.Sheet.Indexed
import Control.Comonad.Sheet.Reference

import Data.Functor.Nested
import Data.List.Indexed
import Data.Stream.Tape
import Data.Numeric.Witness.Peano

import Control.Comonad
import Control.Applicative
import Data.Distributive
import Data.Traversable
import Data.Numeric.Function
import Data.Function

-- | Take a container of functions referencing containers of values and return the fixed-point: a container of values.
evaluate :: (ComonadApply w) => w (w a -> a) -> w a
evaluate fs = fix $ (fs <@>) . duplicate

-- | Given a relative or absolute position, extract from a sheet the value at that location.
cell :: (Comonad w, Go r w) => RefList r -> w a -> a
cell = (extract .) . go

-- | Given a list of relative or absolute positions, extract from a sheet the values at those locations.
cells :: (Traversable t, Comonad w, Go r w) => t (RefList r) -> w a -> t a
cells = traverse cell

-- | Given a default value and an insertable container of values, construct a sheet containing those values.
sheet :: ( InsertNested l (Nested ts) , Applicative (Nested ts)
         , DimensionalAs x (Nested ts a) , AsDimensionalAs x (Nested ts a) ~ l a )
         => a -> x -> Nested ts a
sheet background functions = insert functions (pure background)

-- | Given an origin index, a default value, and an insertable container of values, construct an indexed sheet.
indexedSheet :: ( InsertNested l (Nested ts) , Applicative (Nested ts)
                , DimensionalAs x (Nested ts a) , AsDimensionalAs x (Nested ts a) ~ l a)
                => Coordinate (NestedCount ts) -> a -> x -> Indexed ts a
indexedSheet i = (Indexed i .) . sheet
