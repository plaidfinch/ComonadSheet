{-# LANGUAGE BangPatterns #-}

module ListZipper
   ( Z1
   , zipper , zipperOf , zipIterate
   , zipL , zipR , zipTo
   , viewL , viewR , view , index , window
   , write , modify , switch
   , insertL , insertR , deleteL , deleteR
   , insertListR , insertListL
   ) where

import Control.Applicative
import Control.Arrow
import Data.List
import Data.Function
import Control.Comonad

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
                       (view  fs $ view  xs)
          (zipWith ($) (viewR fs) (viewR xs))
   -- In the case of bounded types, the (toEnum 0) might be a problem; use zipperOf to specify a custom starting index for the zipper
   pure = zipperOf (toEnum 0)

instance (Ord i, Enum i) => Comonad (Z1 i) where
   extract   = view
   duplicate = zipIterate zipL zipR <$> index <*> id

zipper :: i -> [a] -> a -> [a] -> Z1 i a
zipper i lefts cursor rights = Z1 i (cycle lefts) cursor (cycle rights)

zipperOf :: i -> a -> Z1 i a
zipperOf = zipIterate id id

zipIterate :: (a -> a) -> (a -> a) -> i -> a -> Z1 i a
zipIterate prev next i current =
   Z1 i <$> (tail . iterate prev)
        <*> id
        <*> (tail . iterate next) $ current

zipL, zipR :: Enum i => Z1 i a -> Z1 i a

zipL (Z1 i (left : lefts) cursor rights) = Z1 (pred i) lefts left (cursor : rights)
zipL _ = error "zipL of non-infinite zipper; the impossible has occurred"

zipR (Z1 i lefts cursor (right : rights)) = Z1 (succ i) (cursor : lefts) right rights
zipR _ = error "zipR of non-infinite zipper; the impossible has occurred"

zipTo :: (Enum i, Ord i) => i -> Z1 i a -> Z1 i a
zipTo i z | i < index z = zipTo i $! zipL z
zipTo i z | i > index z = zipTo i $! zipR z
zipTo i z | otherwise   = z

viewL :: Z1 i a -> [a]
viewL (Z1 _ lefts _ _) = lefts

viewR :: Z1 i a -> [a]
viewR (Z1 _ _ _ rights) = rights

view :: Z1 i a -> a
view (Z1 _ _ cursor _) = cursor

index :: Z1 i a -> i
index (Z1 i _ _ _) = i

write :: a -> Z1 i a -> Z1 i a
write cursor (Z1 i lefts _ rights) = Z1 i lefts cursor rights

switch :: Z1 i a -> Z1 i a
switch (Z1 i lefts cursor rights) = Z1 i rights cursor lefts

modify :: (a -> a) -> Z1 i a -> Z1 i a
modify f = write <$> f . view <*> id

window :: Int -> Int -> Z1 i a -> [a]
window leftCount rightCount =
   (++) <$> reverse . take leftCount . viewL
        <*> ((:) <$> view <*> take rightCount . viewR)

insertR, insertL :: a -> Z1 i a -> Z1 i a
insertR x (Z1 i lefts cursor rights) = Z1 i lefts x (cursor : rights)
insertL x (Z1 i lefts cursor rights) = Z1 i (cursor : lefts) x rights

insertListR, insertListL :: [a] -> Z1 i a -> Z1 i a
insertListR list (Z1 i lefts cursor rights) =
   Z1 i lefts (head list) (tail list ++ cursor : rights)
insertListL list (Z1 i lefts cursor rights) =
   Z1 i (tail list ++ cursor : lefts) (head list) rights

deleteL, deleteR :: Z1 i a -> Z1 i a
deleteL (Z1 i (left : lefts) cursor rights)  = Z1 i lefts left rights
deleteR (Z1 i lefts cursor (right : rights)) = Z1 i lefts right rights
