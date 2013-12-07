module PlaneZipper where

import ListZipper
import NumericInstances

import Control.Applicative
import Control.Arrow hiding (left,right)
import Data.List
import Control.Comonad

-- | 2-dimensional zippers are nested list zippers
newtype Z2 c r a = Z2 { fromZ2 :: Z1 r (Z1 c a) }

wrapZ2 :: (Z1 r (Z1 c a) -> Z1 s (Z1 d b)) -> Z2 c r a -> Z2 d s b
wrapZ2 f = Z2 . f . fromZ2

instance Functor (Z2 c r) where
   fmap = wrapZ2 . fmap . fmap

instance (Ord c, Ord r, Enum c, Enum r) => Applicative (Z2 c r) where
   fs <*> xs = Z2 $ (fmap (<*>) . fromZ2 $ fs) <*> (fromZ2 xs)
   pure = Z2 . zipperOf (toEnum 0) . zipperOf (toEnum 0)

instance (Ord c, Ord r, Enum c, Enum r) => Comonad (Z2 c r) where
   extract = viewCell
   duplicate = wrapZ2 $ fmap duplicateVert . duplicate
      where duplicateVert = zipIterate left right <$> index . view <*> Z2

above, up :: Enum r => Z2 c r a -> Z2 c r a
above = wrapZ2 $ zipL
up    = above

below, down :: Enum r => Z2 c r a -> Z2 c r a
below = wrapZ2 $ zipR
down  = below

left :: Enum c => Z2 c r a -> Z2 c r a
left = wrapZ2 $ fmap zipL

right :: Enum c => Z2 c r a -> Z2 c r a
right = wrapZ2 $ fmap zipR

zipToCol :: (Ord c, Enum c) => c -> Z2 c r a -> Z2 c r a
zipToCol c z | c < fst (coords z) = zipToCol c $! left  z
zipToCol c z | c > fst (coords z) = zipToCol c $! right z
zipToCol c z | otherwise          = z

zipToRow :: (Ord r, Enum r) => r -> Z2 c r a -> Z2 c r a
zipToRow = wrapZ2 . zipTo

zipToCell :: (Ord c, Ord r, Enum c, Enum r) => (c,r) -> Z2 c r a -> Z2 c r a
zipToCell (c,r) = zipToCol c . zipToRow r

viewCell :: Z2 c r a -> a
viewCell = view . view . fromZ2

window2D :: Int -> Int -> Int -> Int -> Z2 c r a -> [[a]]
window2D u d l r = window u d . fmap (window l r) . fromZ2

rectangle :: (Integral c, Integral r) => (c,r) -> (c,r) -> Z2 c r a -> [[a]]
rectangle (c,r) (c',r') = fmap (genericTake width  . viewR)
                             . (genericTake height . viewR)
                             . fromZ2
                             . zipToCell (c - 1,r - 1)
   where width  = toInteger $ abs (c - c')
         height = toInteger $ abs (r - r')

coords :: Z2 c r a -> (c,r)
coords = (col &&& row)

col :: Z2 c r a -> c
col = index . view . fromZ2

row :: Z2 c r a -> r
row = index . fromZ2

writeCell :: a -> Z2 c r a -> Z2 c r a
writeCell a = wrapZ2 $ modify (write a)

modifyCell :: (a -> a) -> Z2 c r a -> Z2 c r a
modifyCell f = writeCell <$> f . viewCell <*> id

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
