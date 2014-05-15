{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Reference where

import Peano
import CountedList
import TaggedList

import Control.Applicative
import Data.List ( intercalate )

import Prelude hiding ( replicate , length )

data RefType = Relative | Absolute

data Ref (t :: RefType) where
   Rel :: Int -> Ref Relative
   Abs :: Int -> Ref Absolute
deriving instance Show (Ref t)

type family Combine a b where
   Combine Relative Absolute = Absolute
   Combine Absolute Relative = Absolute
   Combine Relative Relative = Relative

class CombineRefs a b where
   combine :: Ref a -> Ref b -> Ref (Combine a b)
instance CombineRefs Absolute Relative where
   combine (Abs a) (Rel b) = Abs (a + b)
instance CombineRefs Relative Absolute where
   combine (Rel a) (Abs b) = Abs (a + b)
instance CombineRefs Relative Relative where
   combine (Rel a) (Rel b) = Rel (a + b)

infixr 4 &
type family a & b where
   (a :-: as) & (b :-: bs) = Combine a b :-: (as & bs)
   Nil        & bs         = bs
   as         & Nil        = as

type RefList = TaggedList Ref

class CombineRefLists as bs where
   (&) :: RefList as -> RefList bs -> RefList (as & bs)
instance (CombineRefs a b, CombineRefLists as bs)
      => CombineRefLists (a :-: as) (b :-: bs) where (a :-: as) & (b :-: bs) = combine a b :-: (as & bs)
instance CombineRefLists Nil        (b :-: bs) where TNil       & bs         = bs
instance CombineRefLists (a :-: as) Nil        where as         & TNil       = as
instance CombineRefLists Nil        Nil        where TNil       & TNil       = TNil

merge :: (Applicative (CountedList n))
      => CountedList n (Ref Relative)
      -> CountedList n (Ref Absolute)
      -> CountedList n (Ref Absolute)
merge rs as = (\(Rel r) (Abs a) -> Abs (r + a)) <$> rs <*> as

diff :: CountedList n (Either (Ref Relative) (Ref Absolute))
     -> CountedList n (Ref Absolute)
     -> CountedList n (Ref Relative)
diff (Left  (Rel r) ::: rs) (Abs i ::: is) = Rel  r      ::: diff rs is
diff (Right (Abs r) ::: rs) (Abs i ::: is) = Rel (r - i) ::: diff rs is
diff CNil _  = CNil
diff _  CNil = CNil

getMovement :: (n ~ PadNatTo n (Length ts), Pad n (Length ts))
            => RefList ts -> CountedList n (Ref Absolute) -> CountedList n (Ref Relative)
getMovement refs coords =
   padTo (count coords) (Left (Rel 0)) (homogenize eitherFromRef refs) `diff` coords

heterogenize :: (a -> f t) -> CountedList n a -> TaggedList f (Replicate n t)
heterogenize _ CNil       = TNil
heterogenize f (x ::: xs) = f x :-: heterogenize f xs

homogenize :: (forall t. f t -> a) -> TaggedList f ts -> CountedList (Length ts) a
homogenize _ TNil       = CNil
homogenize f (x :-: xs) = f x ::: homogenize f xs

eitherFromRef :: Ref t -> Either (Ref Relative) (Ref Absolute)
eitherFromRef (Rel r) = Left  (Rel r)
eitherFromRef (Abs a) = Right (Abs a)

dimensional :: Natural (Succ n) -> Ref t -> RefList (Tack t (Replicate n Relative))
dimensional (Succ n) a = tack a (heterogenize id (replicate n (Rel 0)))

instance Show (RefList ts) where
   showsPrec p xs = showParen (p > 10) $
      (showString $ ( intercalate " :-: "
                    $ map (either show show)
                    $ unCount
                    $ homogenize eitherFromRef xs ) ++ " :-: TNil")
