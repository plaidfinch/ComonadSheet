{-# LANGUAGE TupleSections, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}

module Generic
   ( Ref(..) , module Data.Monoid, module Control.Applicative, module Control.Comonad
   , AnyZipper , Zipper1 , Zipper2 , Zipper3
   , AnyRef , AbsoluteRef , Ref1 , Ref2 , Ref3
   , index , view , zipL , zipR , zipU , zipD , zipI , zipO
   , rightBy , leftBy , atCol , belowBy , aboveBy , atRow , inwardBy , outwardBy , atlev
   , go , at , here , above , below , left , right , inward , outward , goto
   , genericZipBy , genericZipTo , genericDeref
   ) where

import Data.Monoid
import Control.Applicative
import Control.Comonad

data Ref x = Abs x | Rel Int deriving (Show, Eq, Ord)

instance Enum x => Monoid (Ref x) where
   mempty                = Rel 0
   Abs x `mappend` Rel y = Abs $ toEnum (fromEnum x + y)
   Rel x `mappend` Abs y = Abs $ toEnum (x + fromEnum y)
   Rel x `mappend` Rel y = Rel (x + y)
   Abs x `mappend` Abs _ = Abs x

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

class Ref3 ref lev | ref -> lev where
   inwardBy  :: Int -> ref
   inwardBy = outwardBy . negate
   outwardBy :: Int -> ref
   outwardBy = inwardBy . negate
   atlev   :: lev -> ref

here :: (Monoid ref, AnyRef ref zipper) => ref
here = mempty

above, below :: Ref2 ref row => ref
above = aboveBy 1
below = belowBy 1

right, left :: Ref1 ref col => ref
right = rightBy 1
left  = leftBy  1

inward, outward :: Ref3 ref lev => ref
inward  = inwardBy 1
outward = outwardBy 1

instance Enum col => Ref1 (Ref col) col where
   rightBy = Rel
   atCol   = Abs

instance (Enum row, Enum col) => Ref1 (Ref col,Ref row) col where
   rightBy = (,mempty) . Rel
   atCol   = (,mempty) . Abs

instance (Enum row, Enum col) => Ref2 (Ref col,Ref row) row where
   belowBy = (mempty,) . Rel
   atRow   = (mempty,) . Abs

instance (Enum row, Enum col, Enum lev) => Ref1 (Ref col,Ref row,Ref lev) col where
   rightBy = (,mempty,mempty) . Rel
   atCol   = (,mempty,mempty) . Abs

instance (Enum row, Enum col, Enum lev) => Ref2 (Ref col,Ref row,Ref lev) row where
   belowBy = (mempty,,mempty) . Rel
   atRow   = (mempty,,mempty) . Abs

instance (Enum row, Enum col, Enum lev) => Ref3 (Ref col,Ref row,Ref lev) lev where
   inwardBy = (mempty,mempty,) . Rel
   atlev    = (mempty,mempty,) . Abs

class AnyRef ref zipper | zipper -> ref where
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

class AbsoluteRef ref tuple | ref -> tuple where
   at :: tuple -> ref
instance AbsoluteRef (Ref col) col where
   at = Abs
instance AbsoluteRef (Ref col,Ref row) (col,row) where
   at (c,r) = (Abs c,Abs r)
instance AbsoluteRef (Ref col, Ref row,Ref lev) (col,row,lev) where
   at (c,r,l) = (Abs c,Abs r,Abs l)

goto :: (AnyRef ref zipper, AbsoluteRef ref tuple) => tuple -> zipper -> zipper
goto = go . at

