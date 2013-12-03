module PlaneZipper where

import ListZipper
import NumericInstances

import Control.Applicative
import Control.Arrow hiding (left,right)
import Data.List
import Control.Comonad

-- | 2-dimensional zippers are nested list zippers
type Z2 c r a = Z1 r (Z1 c a)

above, up :: Enum r => Z2 c r a -> Z2 c r a
above = zipL
up    = above

below, down :: Enum r => Z2 c r a -> Z2 c r a
below = zipR
down  = below

left :: Enum c => Z2 c r a -> Z2 c r a
left = fmap zipL

right :: Enum c => Z2 c r a -> Z2 c r a
right = fmap zipR

zipToCol :: (Ord c, Enum c) => c -> Z2 c r a -> Z2 c r a
zipToCol c z | c < fst (coords z) = zipToCol c $! left  z
zipToCol c z | c > fst (coords z) = zipToCol c $! right z
zipToCol c z | otherwise          = z

zipToRow :: (Ord r, Enum r) => r -> Z2 c r a -> Z2 c r a
zipToRow = zipTo

zipToCell :: (Ord c, Ord r, Enum c, Enum r) => (c,r) -> Z2 c r a -> Z2 c r a
zipToCell (c,r) = zipToCol c . zipToRow r

viewCell :: Z2 c r a -> a
viewCell = view . view

window2D :: Int -> Int -> Int -> Int -> Z2 c r a -> [[a]]
window2D u d l r = window u d . fmap (window l r)

rectangle :: (Integral c, Integral r) => (c,r) -> (c,r) -> Z2 c r a -> [[a]]
rectangle (c,r) (c',r') = fmap (genericTake width  . viewR)
                             . (genericTake height . viewR)
                             . zipToCell (c - 1,r - 1)
   where width  = toInteger $ abs (c - c')
         height = toInteger $ abs (r - r')

coords :: Z2 c r a -> (c,r)
coords = (col &&& row)

col :: Z2 c r a -> c
col = index . view

row :: Z2 c r a -> r
row = index

writeCell :: a -> Z2 c r a -> Z2 c r a
writeCell a = modify (write a)

modifyCell :: (a -> a) -> Z2 c r a -> Z2 c r a
modifyCell f = writeCell <$> f . viewCell <*> id

modifyRow :: (Z1 c a -> Z1 c a) -> Z2 c r a -> Z2 c r a
modifyRow = modify

modifyCol :: (Enum r, Ord r) => (Z1 r a -> Z1 r a) -> Z2 c r a -> Z2 c r a
modifyCol f = writeCol <$> f . fmap view <*> id

writeRow :: Z1 c a -> Z2 c r a -> Z2 c r a
writeRow = write

writeCol :: (Ord r, Enum r) => Z1 r a -> Z2 c r a -> Z2 c r a
writeCol c plane = write <$> c <*> plane

insertRowU, insertRowD :: Z1 c a -> Z2 c r a -> Z2 c r a
insertRowU = insertL
insertRowD = insertR

deleteRowD, deleteRowU :: Z2 c r a -> Z2 c r a
deleteRowD = deleteR
deleteRowU = deleteL

insertColL, insertColR :: (Ord r, Enum r) => Z1 r a -> Z2 c r a -> Z2 c r a
insertColL c plane = insertL <$> c <*> plane
insertColR c plane = insertR <$> c <*> plane

deleteColL, deleteColR :: (Ord r, Enum r) => Z2 c r a -> Z2 c r a
deleteColL = fmap deleteL
deleteColR = fmap deleteR

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
