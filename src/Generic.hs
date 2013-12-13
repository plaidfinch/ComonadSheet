{-# LANGUAGE TupleSections, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}

module Generic
   ( Ref(..)
   , module Control.Applicative, module Control.Comonad
   , AnyZipper , Zipper1 , Zipper2 , Zipper3 , Zipper4
   , RefOf , AnyRef , Ref1 , Ref2 , Ref3 , Ref4
   , index , view , zipL , zipR , zipU , zipD , zipI , zipO , zipA , zipK
   , rightBy , leftBy , belowBy , aboveBy , inwardBy , outwardBy , anaBy , kataBy
   , atCol , atRow , atLevel , atSpace
   , (&) , go , at , here , goto
   , above , below , left , right , inward , outward , ana , kata
   , genericZipBy , genericZipTo , genericDeref
   ) where

import Data.Monoid
import Control.Applicative
import Control.Comonad

data Ref x = Abs x | Rel Int deriving (Show, Eq, Ord)

infixl 6 &

class AnyRef ref tuple | ref -> tuple where
   at   :: tuple -> ref
   here :: ref
   (&)  :: ref -> ref -> ref

instance Enum col => AnyRef (Ref col) col where
   here          = Rel 0
   at            = Abs
   Abs x & Rel y = Abs $ toEnum (fromEnum x + y)
   Rel x & Abs y = Abs $ toEnum (x + fromEnum y)
   Rel x & Rel y = Rel (x + y)
   Abs _ & Abs y = Abs y

instance (Enum col, Enum row) => AnyRef (Ref col,Ref row) (col,row) where
   here            = (here,here)
   at (c,r)        = (at c,at r)
   (c,r) & (c',r') = (c & c',r & r')

instance (Enum col, Enum row, Enum level) => AnyRef (Ref col, Ref row,Ref level) (col,row,level) where
   here                 = (here,here,here)
   at (c,r,l)           = (at c,at r,at l)
   (c,r,l) & (c',r',l') = (c & c',r & r',l & l')

instance (Enum col, Enum row, Enum level, Enum space) => AnyRef (Ref col, Ref row,Ref level,Ref space) (col,row,level,space) where
   here                      = (here,here,here,here)
   at (c,r,l,s)              = (at c,at r,at l,at s)
   (c,r,l,s) & (c',r',l',s') = (c & c',r & r',l & l',s & s')

goto :: (RefOf ref zipper, AnyRef ref tuple) => tuple -> zipper -> zipper
goto = go . at

class Ref1 ref col | ref -> col where
   rightBy :: Int -> ref
   rightBy = leftBy . negate
   leftBy  :: Int -> ref
   leftBy = rightBy . negate
   atCol   :: col -> ref

class Ref2 ref row | ref -> row where
   belowBy :: Int -> ref
   belowBy = aboveBy . negate
   aboveBy :: Int -> ref
   aboveBy = belowBy . negate
   atRow   :: row -> ref

class Ref3 ref level | ref -> level where
   inwardBy  :: Int -> ref
   inwardBy = outwardBy . negate
   outwardBy :: Int -> ref
   outwardBy = inwardBy . negate
   atLevel   :: level -> ref

class Ref4 ref level | ref -> level where
   anaBy  :: Int -> ref
   anaBy = kataBy . negate
   kataBy :: Int -> ref
   kataBy = anaBy . negate
   atSpace :: level -> ref

above, below :: Ref2 ref row => ref
above = aboveBy 1
below = belowBy 1

right, left :: Ref1 ref col => ref
right = rightBy 1
left  = leftBy  1

inward, outward :: Ref3 ref level => ref
inward  = inwardBy  1
outward = outwardBy 1

ana, kata :: Ref4 ref space => ref
ana  = anaBy  1
kata = kataBy 1

instance Enum col => Ref1 (Ref col) col where
   rightBy = Rel
   atCol   = Abs

instance (Enum row, Enum col) => Ref1 (Ref col,Ref row) col where
   rightBy = (,here) . Rel
   atCol   = (,here) . Abs

instance (Enum row, Enum col) => Ref2 (Ref col,Ref row) row where
   belowBy = (here,) . Rel
   atRow   = (here,) . Abs

instance (Enum row, Enum col, Enum level) => Ref1 (Ref col,Ref row,Ref level) col where
   rightBy = (,here,here) . Rel
   atCol   = (,here,here) . Abs

instance (Enum row, Enum col, Enum level) => Ref2 (Ref col,Ref row,Ref level) row where
   belowBy = (here,,here) . Rel
   atRow   = (here,,here) . Abs

instance (Enum row, Enum col, Enum level) => Ref3 (Ref col,Ref row,Ref level) level where
   inwardBy = (here,here,) . Rel
   atLevel  = (here,here,) . Abs

instance (Enum row, Enum col, Enum level, Enum space) => Ref1 (Ref col,Ref row,Ref level,Ref space) col where
   rightBy = (,here,here,here) . Rel
   atCol   = (,here,here,here) . Abs

instance (Enum row, Enum col, Enum level, Enum space) => Ref2 (Ref col,Ref row,Ref level,Ref space) row where
   belowBy = (here,,here,here) . Rel
   atRow   = (here,,here,here) . Abs

instance (Enum row, Enum col, Enum level, Enum space) => Ref3 (Ref col,Ref row,Ref level,Ref space) level where
   inwardBy = (here,here,,here) . Rel
   atLevel  = (here,here,,here) . Abs

instance (Enum row, Enum col, Enum level, Enum space) => Ref4 (Ref col,Ref row,Ref level,Ref space) space where
   anaBy   = (here,here,here,) . Rel
   atSpace = (here,here,here,) . Abs

class RefOf ref zipper | zipper -> ref where
   go :: ref -> zipper -> zipper

genericZipBy :: (z -> z) -> (z -> z) -> Int -> z -> z
genericZipBy zl zr i | i < 0     = genericZipBy zl zr (succ i) . zl
genericZipBy zl zr i | i > 0     = genericZipBy zl zr (pred i) . zr
genericZipBy zl zr i | otherwise = id

genericZipTo :: (Ord i) => (z -> z) -> (z -> z) -> (z -> i) -> i -> z -> z
genericZipTo zl zr idx i z | i < idx z = genericZipTo zl zr idx i . zl $ z
genericZipTo zl zr idx i z | i > idx z = genericZipTo zl zr idx i . zr $ z
genericZipTo zl zr idx i z | otherwise = z

genericDeref :: (Ord i) => (z -> z) -> (z -> z) -> (z -> i) -> Ref i -> z -> z
genericDeref zl zr idx ref =
   case ref of
      Rel i -> relative i
      Abs x -> absolute x
   where relative = genericZipBy zl zr
         absolute = genericZipTo zl zr idx

class AnyZipper z i a | z -> i a where
   index :: z -> i
   view  :: z -> a

class Zipper1 z where
   zipL :: z -> z
   zipR :: z -> z

class Zipper2 z where
   zipU :: z -> z
   zipD :: z -> z

class Zipper3 z where
   zipI :: z -> z
   zipO :: z -> z

class Zipper4 z where
   zipA :: z -> z
   zipK :: z -> z
