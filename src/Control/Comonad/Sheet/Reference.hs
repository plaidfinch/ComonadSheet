{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Control.Comonad.Sheet.Reference where

import Data.Numeric.Witness.Peano
import Data.List.Indexed

import Control.Applicative
import Data.List ( intercalate )

import Prelude hiding ( replicate , length )

-- | A 'Ref' is either absolute or relative. This type exists solely to be lifted to the type/kind level.
data RefType = Relative | Absolute

-- | A @Ref@ is either absolute or relative. Either holds an @Int@, but their semantics differ.
data Ref (t :: RefType) where
   Rel :: Int -> Ref Relative
   Abs :: Int -> Ref Absolute
deriving instance Show (Ref t)

-- | Extract the raw @Int@ from a @Ref@. Use this sparingly, as every time you do, you risk accidentally converting
--   to the other variety of @Ref@ if you're not careful.
getRef :: Ref t -> Int
getRef (Abs x) = x
getRef (Rel x) = x

instance Enum (Ref Relative) where
   fromEnum (Rel r) = r
   toEnum = Rel

instance Enum (Ref Absolute) where
   fromEnum (Abs r) = r
   toEnum = Abs

-- | Two absolute references cannot be combined into a single reference, but relative and absolute references can be
--   combined to form an absolute reference, and two relative references can be combined into one relative reference.
type family Combine a b where
   Combine Relative Absolute = Absolute
   Combine Absolute Relative = Absolute
   Combine Relative Relative = Relative

-- | Combine @Ref@s which can be meaningfully combined. Note the absence of the absolute-absolute case; there's no
--   good meaning for combining two absolute references, so it is statically prohibited.
class CombineRefs a b where
   combine :: Ref a -> Ref b -> Ref (Combine a b)
instance CombineRefs Absolute Relative where
   combine (Abs a) (Rel b) = Abs (a + b)
instance CombineRefs Relative Absolute where
   combine (Rel a) (Abs b) = Abs (a + b)
instance CombineRefs Relative Relative where
   combine (Rel a) (Rel b) = Rel (a + b)

-- | A @RefList@ is a list of @Ref@s, each of which may be individually relative or absolute.
type RefList = ConicList Ref

infixr 4 &
type family a & b where
   (a :-: as) & (b :-: bs) = Combine a b :-: (as & bs)
   Nil        & bs         = bs
   as         & Nil        = as

-- | We can combine lists of references if their corresponding elements can be combined. When combining two lists of
--   references, any trailing elements from the longer list will be preserved at the end; this is /unlike/ the behavior
--   of, e.g., @zip@.
class CombineRefLists as bs where
   (&) :: RefList as -> RefList bs -> RefList (as & bs)
instance (CombineRefs a b, CombineRefLists as bs)
      => CombineRefLists (a :-: as) (b :-: bs) where (a :-: as) & (b :-: bs) = combine a b :-: (as & bs)
instance CombineRefLists Nil        (b :-: bs) where ConicNil & bs           = bs
instance CombineRefLists (a :-: as) Nil        where as       & ConicNil     = as
instance CombineRefLists Nil        Nil        where ConicNil & ConicNil     = ConicNil

-- | Given a homogeneous list of length n containing relative references, we can merge those relative positions with a
--   homogeneous list of absolute references. This yields another list of absolute references.
merge :: (ReifyNatural n)
      => CountedList n (Ref Relative)
      -> CountedList n (Ref Absolute)
      -> CountedList n (Ref Absolute)
merge rs as = (\(Rel r) (Abs a) -> Abs (r + a)) <$> rs <*> as

-- | Finds the relative movement necessary to move from a given absolute coordinate to the location specified by a
--   list of relative and absolute coordinates.
diff :: CountedList n (Either (Ref Relative) (Ref Absolute))
     -> CountedList n (Ref Absolute)
     -> CountedList n (Ref Relative)
diff (Left  (Rel r) ::: rs) (Abs i ::: is) = Rel  r      ::: diff rs is
diff (Right (Abs r) ::: rs) (Abs i ::: is) = Rel (r - i) ::: diff rs is
diff CountedNil _  = CountedNil
diff _  CountedNil = CountedNil

-- | Given a list of relative and absolute references (an n-dimensional reference) and an n-dimensional coordinate,
--   we can obtain the relative movement necessary to get from the coordinate to the location specified by the 
--   references given.
getMovement :: (Length ts <= n, ((n - Length ts) + Length ts) ~ n)
            => RefList ts -> CountedList n (Ref Absolute) -> CountedList n (Ref Relative)
getMovement refs coords =
   padTo (count coords) (Left (Rel 0)) (homogenize eitherFromRef refs) `diff` coords

-- | Given a @Ref@, forget the type-level information about whether it's absolute or relative by casting it into an
--   @Either@ type where the @Left@ branch holds a relative reference or the @Right@ holds an absolute one.
eitherFromRef :: Ref t -> Either (Ref Relative) (Ref Absolute)
eitherFromRef (Rel r) = Left  (Rel r)
eitherFromRef (Abs a) = Right (Abs a)

-- | Given a number /n/ greater than zero and some reference, prepend (n - 1) relative references of value zero to the
--   reference given, thus creating an n-dimensional reference where the original reference refers to the nth dimension.
dimensional :: Natural (Succ n) -> Ref t -> RefList (Tack t (Replicate n Relative))
dimensional (Succ n) a = tack a (heterogenize id (replicate n (Rel 0)))

instance Show (RefList ts) where
   showsPrec p xs = showParen (p > 10) $
      (showString $ ( intercalate " :-: "
                    $ map (either show show)
                    $ unCount
                    $ homogenize eitherFromRef xs ) ++ " :-: ConicNil")
