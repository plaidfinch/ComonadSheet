module ListZipper
   ( ListZipper
   , listZipper
   , zipL , zipR
   , insertL , insertR
   , deleteL , deleteR
   , viewL , viewR , view , window
   ) where

import Control.Applicative
import Control.Arrow
import Data.List

data ListZipper a = LZ [a] a [a]

instance Functor ListZipper where
   fmap f = LZ <$> fmap f . viewL
               <*>      f . view
               <*> fmap f . viewR

instance Applicative ListZipper where
   fs <*> xs =
      LZ (zipWith ($) (viewL fs) (viewL xs))
                      (view  fs $ view  xs)
         (zipWith ($) (viewR fs) (viewR xs))
   pure = listZipper <$> (:[]) <*> id <*> (:[])

listZipper :: [a] -> a -> [a] -> ListZipper a
listZipper lefts cursor rights = LZ (cycle lefts) cursor (cycle rights)

zipL :: ListZipper a -> ListZipper a
zipL (LZ (left : lefts) cursor rights) =
   LZ lefts left (cursor : rights)

zipR :: ListZipper a -> ListZipper a
zipR (LZ lefts cursor (right : rights)) =
   LZ (cursor : lefts) right rights

viewL :: ListZipper a -> [a]
viewL (LZ lefts cursor rights) = lefts

viewR :: ListZipper a -> [a]
viewR (LZ lefts cursor rights) = rights

view :: ListZipper a -> a
view (LZ lefts cursor rights) = cursor

write :: a -> ListZipper a -> ListZipper a
write cursor (LZ lefts _ rights) = LZ lefts cursor rights

window :: Int -> Int -> ListZipper a -> [a]
window leftCount rightCount =
   (++) <$> reverse . take leftCount . viewL
        <*> ((:) <$> view
                 <*> take rightCount . viewR)

insertR :: a -> ListZipper a -> ListZipper a
insertR x (LZ lefts cursor rights) = LZ lefts x (cursor : rights)

insertL :: a -> ListZipper a -> ListZipper a
insertL x (LZ lefts cursor rights) = LZ (cursor : lefts) x rights

insertListR :: [a] -> ListZipper a -> ListZipper a
insertListR list (LZ lefts cursor rights) =
   LZ lefts (head list) (tail list ++ cursor : rights)

insertListL :: [a] -> ListZipper a -> ListZipper a
insertListL list (LZ lefts cursor rights) =
   LZ (tail list ++ cursor : lefts) (head list) rights

deleteL :: ListZipper a -> ListZipper a
deleteL (LZ (left : lefts) cursor rights) = LZ lefts left rights

deleteR :: ListZipper a -> ListZipper a
deleteR (LZ lefts cursor (right : rights)) = LZ lefts right rights

zipLoeb :: ListZipper (ListZipper a -> a) -> ListZipper a
zipLoeb fs =
   go where go = LZ <$> zipWith ($) (viewL fs) . tail . iterate zipL
                    <*>              view  fs
                    <*> zipWith ($) (viewR fs) . tail . iterate zipR $ go
