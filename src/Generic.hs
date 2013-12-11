{-# LANGUAGE TupleSections, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}

module Generic where

import Data.Monoid

data Ref x = Abs Int x | Rel Int deriving (Show, Eq, Ord)

instance Monoid (Ref x) where
   mempty                    = Rel 0
   Abs x y `mappend` Rel z   = Abs (x + z) y
   Rel x   `mappend` Abs y z = Abs (x + y) z
   Rel x   `mappend` Rel y   = Rel (x + y)
   Abs y z `mappend` Abs _ _ = Abs y z

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

class Ref3 ref level | ref -> level where
   inwardBy  :: Int -> ref
   inwardBy = outwardBy . negate
   outwardBy :: Int -> ref
   outwardBy = inwardBy . negate
   atLevel   :: level -> ref

here :: AnyRef ref zipper => ref
here = mempty

above, below :: Ref2 ref r => ref
above = aboveBy 1
below = belowBy 1

right, left :: Ref1 ref c => ref
right = rightBy 1
left  = leftBy  1

inward, outward :: Ref3 ref l => ref
inward  = inwardBy 1
outward = outwardBy 1

instance Ref1 (Ref c) c where
   rightBy = Rel
   atCol   = Abs 0

instance Ref1 (Ref c,Ref r) c where
   rightBy = (,mempty) . Rel
   atCol   = (,mempty) . Abs 0

instance Ref2 (Ref c,Ref r) r where
   belowBy = (mempty,) . Rel
   atRow   = (mempty,) . Abs 0

instance Ref3 (Ref c,Ref r,Ref l) l where
   inwardBy = (mempty,mempty,) . Rel
   atLevel  = (mempty,mempty,) . Abs 0

instance Ref1 (Ref c,Ref r,Ref l) c where
   rightBy = (,mempty,mempty) . Rel
   atCol   = (,mempty,mempty) . Abs 0

instance Ref2 (Ref c,Ref r,Ref l) r where
   belowBy = (mempty,,mempty) . Rel
   atRow   = (mempty,,mempty) . Abs 0

class Monoid ref => AnyRef ref zipper | zipper -> ref where
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
      Rel i   -> relative i
      Abs i x -> relative i . absolute x
   where relative = genericZipBy zl zr
         absolute = genericZipTo zl zr idx

class AbsoluteRef ref tuple | ref -> tuple where
   at :: tuple -> ref
instance AbsoluteRef (Ref c) c where
   at = Abs 0
instance AbsoluteRef (Ref c,Ref r) (c,r) where
   at (c,r) = (Abs 0 c,Abs 0 r)
instance AbsoluteRef (Ref c, Ref r,Ref l) (c,r,l) where
   at (c,r,l) = (Abs 0 c,Abs 0 r,Abs 0 l)

goto :: (AnyRef ref zipper, AbsoluteRef ref tuple) => tuple -> zipper -> zipper
goto = go . at

