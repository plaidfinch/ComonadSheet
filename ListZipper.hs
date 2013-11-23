{-# LANGUAGE BangPatterns #-}

module ListZipper
   ( Z1
   , zipper , zipperOf
   , zipL , zipR , zipTo
   , viewL , viewR , view , index , window
   , write , modify
   , insertL , insertR , deleteL , deleteR
   , insertListR , insertListL
   , zipApply
   ) where

import Control.Applicative
import Control.Arrow
import Data.List
import Data.Function

-- 1-D zippers...

-- | One-dimensional list zipper
data Z1 i a = ILZ !i [a] a [a]

instance Functor (Z1 i) where
   fmap f = ILZ <$> index
                <*> fmap f . viewL
                <*>      f . view
                <*> fmap f . viewR

instance (Enum i, Ord i) => Applicative (Z1 i) where
   fs <*> xs =
      ILZ (index fs)
          (zipWith ($) (viewL fs) (viewL xs))
                       (view  fs $ view  xs)
          (zipWith ($) (viewR fs) (viewR xs))
   -- In the case of bounded types, the (toEnum 0) might be a problem; use zipperOf to specify a custom starting index for the zipper
   pure = zipperOf (toEnum 0)

zipper :: i -> [a] -> a -> [a] -> Z1 i a
zipper i lefts cursor rights = ILZ i (cycle lefts) cursor (cycle rights)

zipperOf :: i -> a -> Z1 i a
zipperOf i a = zipper i [a] a [a]

zipL :: Enum i => Z1 i a -> Z1 i a
zipL (ILZ i (left : lefts) cursor rights) =
   ILZ (pred i) lefts left (cursor : rights)
zipL _ = error "zipL of non-infinite zipper; the impossible has occurred"

zipR :: Enum i => Z1 i a -> Z1 i a
zipR (ILZ i lefts cursor (right : rights)) =
   ILZ (succ i) (cursor : lefts) right rights
zipR _ = error "zipR of non-infinite zipper; the impossible has occurred"

zipTo :: (Enum i, Ord i) => i -> Z1 i a -> Z1 i a
zipTo i z | i < index z = zipTo i $! zipL z
zipTo i z | i > index z = zipTo i $! zipR z
zipTo i z | otherwise   = z

viewL :: Z1 i a -> [a]
viewL (ILZ _ lefts _ _) = lefts

viewR :: Z1 i a -> [a]
viewR (ILZ _ _ _ rights) = rights

view :: Z1 i a -> a
view (ILZ _ _ cursor _) = cursor

index :: Z1 i a -> i
index (ILZ i _ _ _) = i

write :: a -> Z1 i a -> Z1 i a
write cursor (ILZ i lefts _ rights) = ILZ i lefts cursor rights

modify :: (a -> a) -> Z1 i a -> Z1 i a
modify f = write <$> f . view <*> id

window :: Int -> Int -> Z1 i a -> [a]
window leftCount rightCount =
   (++) <$> reverse . take leftCount . viewL
        <*> ((:) <$> view
                 <*> take rightCount . viewR)

insertR :: a -> Z1 i a -> Z1 i a
insertR x (ILZ i lefts cursor rights) =
   ILZ i lefts cursor (x : rights)

insertL :: a -> Z1 i a -> Z1 i a
insertL x (ILZ i lefts cursor rights) =
   ILZ i (x : lefts) cursor rights

insertListR :: [a] -> Z1 i a -> Z1 i a
insertListR list (ILZ i lefts cursor rights) =
   ILZ i lefts cursor (list ++ rights)

insertListL :: [a] -> Z1 i a -> Z1 i a
insertListL list (ILZ i lefts cursor rights) =
   ILZ i (list ++ lefts) cursor rights

deleteL :: Z1 i a -> Z1 i a
deleteL (ILZ i (left : lefts) cursor rights) = ILZ i lefts left rights

deleteR :: Z1 i a -> Z1 i a
deleteR (ILZ i lefts cursor (right : rights)) = ILZ i lefts right rights

zipApply :: Enum i => Z1 j (Z1 i a -> b) -> Z1 i a -> Z1 i b
zipApply fs =
   ILZ <$> index
       <*> zipWith ($) (viewL fs) . tail . iterate zipL
       <*>              view  fs
       <*> zipWith ($) (viewR fs) . tail . iterate zipR

zipLoeb :: Enum i => Z1 i (Z1 i a -> a) -> Z1 i a
zipLoeb = fix . zipApply
