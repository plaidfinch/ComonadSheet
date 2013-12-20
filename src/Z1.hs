{-# LANGUAGE BangPatterns, FlexibleInstances, MultiParamTypeClasses, MultiWayIf #-}

module Z1
   ( Z1
   , zipper , zipperOf , zipIterate
   , viewL , viewR , view , segment
   , write , modify , switch
   , insertL , insertR , deleteL , deleteR
   , insertListR , insertListL
   ) where

import Generic

-- 1-D zippers...

-- | One-dimensional list zipper
data Z1 i a = Z1 !i [a] a [a]

instance Functor (Z1 i) where
   fmap f = Z1 <$> index
               <*> fmap f . viewL
               <*>      f . view
               <*> fmap f . viewR

instance (Enum i, Ord i) => Applicative (Z1 i) where
   fs <*> xs =
      Z1 (index fs)
         (zipWith ($) (viewL fs) (viewL xs))
         (view fs $ view xs)
         (zipWith ($) (viewR fs) (viewR xs))
   -- In the case of bounded types, the (toEnum 0) might be a problem; use zipperOf to specify a custom starting index for the zipper
   pure = zipperOf (toEnum 0)

instance (Ord i, Enum i) => Comonad (Z1 i) where
   extract   = view
   duplicate = zipIterate zipL zipR <$> index <*> id

instance AnyZipper (Z1 i a) i a where
   index (Z1 i _ _ _) = i
   view  (Z1 _ _ c _) = c

instance (Enum i, Ord i) => Zipper1 (Z1 i a) where
   zipL (Z1 i (left : lefts) cursor rights) =
      Z1 (pred i) lefts left (cursor : rights)
   zipL _ = error "zipL of non-infinite zipper; the impossible has occurred"

   zipR (Z1 i lefts cursor (right : rights)) =
      Z1 (succ i) (cursor : lefts) right rights
   zipR _ = error "zipR of non-infinite zipper; the impossible has occurred"

instance (Ord c, Enum c) => RefOf (Ref c) (Z1 c a) where
   go = genericDeref zipL zipR index

zipper :: i -> [a] -> a -> [a] -> Z1 i a
zipper i lefts cursor rights = Z1 i (cycle lefts) cursor (cycle rights)

zipperOf :: i -> a -> Z1 i a
zipperOf = zipIterate id id

zipIterate :: (a -> a) -> (a -> a) -> i -> a -> Z1 i a
zipIterate prev next i current =
   Z1 i <$> (tail . iterate prev)
        <*> id
        <*> (tail . iterate next) $ current

viewL :: Z1 i a -> [a]
viewL (Z1 _ lefts _ _) = lefts

viewR :: Z1 i a -> [a]
viewR (Z1 _ _ _ rights) = rights

write :: a -> Z1 i a -> Z1 i a
write cursor (Z1 i lefts _ rights) = Z1 i lefts cursor rights

switch :: Z1 i a -> Z1 i a
switch (Z1 i lefts cursor rights) = Z1 i rights cursor lefts

modify :: (a -> a) -> Z1 i a -> Z1 i a
modify f = write <$> f . view <*> id

segment :: (Enum i, Ord i) => Ref i -> Ref i -> Z1 i a -> [a]
segment ref1 ref2 z =
   if | dist > 0  -> take    dist  . viewR $ go left loc1
      | dist < 0  -> take (- dist) . viewR $ go left loc2
      | otherwise -> []
   where loc1 = go ref1 z
         loc2 = go ref2 z
         dist = (fromEnum $ index loc2) - (fromEnum $ index loc1)

insertR, insertL :: a -> Z1 i a -> Z1 i a
insertR x (Z1 i lefts cursor rights) = Z1 i lefts x (cursor : rights)
insertL x (Z1 i lefts cursor rights) = Z1 i (cursor : lefts) x rights

insertListR, insertListL :: [a] -> Z1 i a -> Z1 i a

insertListR [] z = z
insertListR list (Z1 i lefts cursor rights) =
   Z1 i lefts (head list) (tail list ++ cursor : rights)

insertListL [] z = z
insertListL list (Z1 i lefts cursor rights) =
   Z1 i (tail list ++ cursor : lefts) (head list) rights

deleteL, deleteR :: Z1 i a -> Z1 i a
deleteL (Z1 i (left : lefts) cursor rights)  = Z1 i lefts left rights
deleteR (Z1 i lefts cursor (right : rights)) = Z1 i lefts right rights
