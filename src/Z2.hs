{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TupleSections #-}

module Z2 where

import Generic
import Z1

import Control.Arrow ( (&&&) )
import Data.List

-- | 2-dimensional zippers are nested list zippers
newtype Z2 c r a = Z2 { fromZ2 :: Z1 r (Z1 c a) }

wrapZ2 :: (Z1 r (Z1 c a) -> Z1 r' (Z1 c' a')) -> Z2 c r a -> Z2 c' r' a'
wrapZ2 = (Z2 .) . (. fromZ2)

instance Functor (Z2 c r) where
   fmap = wrapZ2 . fmap . fmap

instance (Ord c, Ord r, Enum c, Enum r) => Applicative (Z2 c r) where
   fs <*> xs = Z2 $ fmap (<*>) (fromZ2 fs) <*> (fromZ2 xs)
   pure      = Z2 . pure . pure

instance (Ord c, Ord r, Enum c, Enum r) => Comonad (Z2 c r) where
   extract   = view
   duplicate = wrapZ2 $
      fmap duplicateHorizontal . duplicate
      where duplicateHorizontal = zipIterate zipL zipR <$> index . view <*> Z2

instance (Ord c, Ord r, Enum c, Enum r) => Zipper1 (Z2 c r a) where
   zipL = wrapZ2 $ fmap zipL
   zipR = wrapZ2 $ fmap zipR

instance (Ord c, Ord r, Enum c, Enum r) => Zipper2 (Z2 c r a) where
   zipU = wrapZ2 zipL
   zipD = wrapZ2 zipR

instance (Ord c, Enum c, Ord r, Enum r) => RefOf (Ref c,Ref r) (Z2 c r a) where
   go (colRef,rowRef) = horizontal . vertical
      where
         horizontal = genericDeref zipL zipR col colRef
         vertical   = genericDeref zipU zipD row rowRef

instance AnyZipper (Z2 c r a) (c,r) a where
   index = (col &&& row)
   view  = view . view . fromZ2

rectangle :: (Integral c, Integral r) => (Ref c,Ref r) -> (Ref c,Ref r) -> Z2 c r a -> [[a]]
rectangle (c,r) (c',r') = fmap (segment c c') . segment r r' . fromZ2

col :: Z2 c r a -> c
col = index . view . fromZ2

row :: Z2 c r a -> r
row = index . fromZ2

writeCell :: a -> Z2 c r a -> Z2 c r a
writeCell a = wrapZ2 $ modify (write a)

modifyCell :: (a -> a) -> Z2 c r a -> Z2 c r a
modifyCell f = writeCell <$> f . view <*> id

modifyRow :: (Z1 c a -> Z1 c a) -> Z2 c r a -> Z2 c r a
modifyRow = wrapZ2 . modify

modifyCol :: (Enum r, Ord r) => (Z1 r a -> Z1 r a) -> Z2 c r a -> Z2 c r a
modifyCol f = writeCol <$> f . fmap view . fromZ2 <*> id

writeRow :: Z1 c a -> Z2 c r a -> Z2 c r a
writeRow = wrapZ2 . write

writeCol :: (Ord r, Enum r) => Z1 r a -> Z2 c r a -> Z2 c r a
writeCol c plane = Z2 $ write <$> c <*> fromZ2 plane

insertRowU, insertRowD :: Z1 c a -> Z2 c r a -> Z2 c r a
insertRowU = wrapZ2 . insertL
insertRowD = wrapZ2 . insertR

deleteRowD, deleteRowU :: Z2 c r a -> Z2 c r a
deleteRowD = wrapZ2 $ deleteR
deleteRowU = wrapZ2 $ deleteL

insertColL, insertColR :: (Ord r, Enum r) => Z1 r a -> Z2 c r a -> Z2 c r a
insertColL c plane = Z2 $ insertL <$> c <*> fromZ2 plane
insertColR c plane = Z2 $ insertR <$> c <*> fromZ2 plane

deleteColL, deleteColR :: (Ord r, Enum r) => Z2 c r a -> Z2 c r a
deleteColL = wrapZ2 $ fmap deleteL
deleteColR = wrapZ2 $ fmap deleteR

insertCellD, insertCellU :: (Ord r, Enum r) => a -> Z2 c r a -> Z2 c r a
insertCellD = modifyCol . insertR
insertCellU = modifyCol . insertL

insertCellR, insertCellL :: (Ord c, Enum c) => a -> Z2 c r a -> Z2 c r a
insertCellR = modifyRow . insertR
insertCellL = modifyRow . insertL

deleteCellD, deleteCellU :: (Ord r, Enum r) => Z2 c r a -> Z2 c r a
deleteCellD = modifyCol deleteR
deleteCellU = modifyCol deleteL

deleteCellR, deleteCellL :: (Ord c, Enum c) => Z2 c r a -> Z2 c r a
deleteCellR = modifyRow deleteR
deleteCellL = modifyRow deleteL

--TODO...
--insertCellsR, insertCellsL, insertCellsU, insertCellsD
--insertColsR, insertColsL, insertRowsU, insertRowsD
