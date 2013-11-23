{-# LANGUAGE BangPatterns #-}

module IndexedListZipper
   ( IndexedListZipper
   , listZipper
   , zipL , zipR , zipTo
   , viewL , viewR , view , index , window
   , write , modify
   , insertL , insertR , deleteL , deleteR
   , insertListR , insertListL
   , zipLoeb
   ) where

import Control.Applicative
import Control.Arrow
import Data.List
import Data.Function

data IndexedListZipper i a = ILZ !i [a] a [a]

instance Functor (IndexedListZipper i) where
   fmap f = ILZ <$> index
                <*> fmap f . viewL
                <*>      f . view
                <*> fmap f . viewR

instance (Enum i, Ord i) => Applicative (IndexedListZipper i) where
   fs <*> xs =
      ILZ (index fs)
          (zipWith ($) (viewL fs) (viewL xs'))
                       (view  fs $ view  xs')
          (zipWith ($) (viewR fs) (viewR xs'))
      where xs' = zipTo (index fs) xs
   -- In the case of bounded types, the (toEnum 0) might be a problem...
   pure = listZipper (toEnum 0) <$> (:[]) <*> id <*> (:[])

listZipper :: i -> [a] -> a -> [a] -> IndexedListZipper i a
listZipper i lefts cursor rights = ILZ i (cycle lefts) cursor (cycle rights)

zipL :: (Enum i) => IndexedListZipper i a -> IndexedListZipper i a
zipL (ILZ i (left : lefts) cursor rights) =
   ILZ (pred i) lefts left (cursor : rights)

zipR :: (Enum i) => IndexedListZipper i a -> IndexedListZipper i a
zipR (ILZ i lefts cursor (right : rights)) =
   ILZ (succ i) (cursor : lefts) right rights

zipTo :: (Enum i, Ord i) => i -> IndexedListZipper i a -> IndexedListZipper i a
zipTo i z | i < index z = zipTo i $! zipL z
zipTo i z | i > index z = zipTo i $! zipR z
zipTo i z | otherwise   = z

viewL :: IndexedListZipper i a -> [a]
viewL (ILZ _ lefts _ _) = lefts

viewR :: IndexedListZipper i a -> [a]
viewR (ILZ _ _ _ rights) = rights

view :: IndexedListZipper i a -> a
view (ILZ _ _ cursor _) = cursor

index :: IndexedListZipper i a -> i
index (ILZ i _ _ _) = i

write :: a -> IndexedListZipper i a -> IndexedListZipper i a
write cursor (ILZ i lefts _ rights) = ILZ i lefts cursor rights

modify :: (a -> a) -> IndexedListZipper i a -> IndexedListZipper i a
modify f = write <$> f . view <*> id

window :: Int -> Int -> IndexedListZipper i a -> [a]
window leftCount rightCount =
   (++) <$> reverse . take leftCount . viewL
        <*> ((:) <$> view
                 <*> take rightCount . viewR)

insertR :: a -> IndexedListZipper i a -> IndexedListZipper i a
insertR x (ILZ i lefts cursor rights) = ILZ i lefts x (cursor : rights)

insertL :: a -> IndexedListZipper i a -> IndexedListZipper i a
insertL x (ILZ i lefts cursor rights) = ILZ i (cursor : lefts) x rights

insertListR :: [a] -> IndexedListZipper i a -> IndexedListZipper i a
insertListR list (ILZ i lefts cursor rights) =
   ILZ i lefts (head list) (tail list ++ cursor : rights)

insertListL :: [a] -> IndexedListZipper i a -> IndexedListZipper i a
insertListL list (ILZ i lefts cursor rights) =
   ILZ i (tail list ++ cursor : lefts) (head list) rights

deleteL :: IndexedListZipper i a -> IndexedListZipper i a
deleteL (ILZ i (left : lefts) cursor rights) = ILZ i lefts left rights

deleteR :: IndexedListZipper i a -> IndexedListZipper i a
deleteR (ILZ i lefts cursor (right : rights)) = ILZ i lefts right rights

zipLoeb :: Enum i => IndexedListZipper i (IndexedListZipper i a -> a) -> IndexedListZipper i a
zipLoeb fs = fix $
   ILZ <$> index
       <*> zipWith ($) (viewL fs) . tail . iterate zipL
       <*>              view  fs
       <*> zipWith ($) (viewR fs) . tail . iterate zipR
