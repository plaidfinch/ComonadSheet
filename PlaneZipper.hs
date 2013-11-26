module PlaneZipper where

import ListZipper
import NumericInstances

import Control.Applicative
import Control.Arrow hiding (left, right)
import Data.Function

-- | 2-dimensional zippers are nested list zippers
type Z2 c r a = Z1 r (Z1 c a)

up :: Enum r => Z2 c r a -> Z2 c r a
up = zipL

down :: Enum r => Z2 c r a -> Z2 c r a
down = zipR

left :: Enum c => Z2 c r a -> Z2 c r a
left = fmap zipL

right :: Enum c => Z2 c r a -> Z2 c r a
right = fmap zipR

zipToCol :: (Ord c, Enum c) => c -> Z2 c r a -> Z2 c r a
zipToCol c z | c < fst (indexCell z) = zipToCol c $! left  z
zipToCol c z | c > fst (indexCell z) = zipToCol c $! right z
zipToCol c z | otherwise             = z

zipToRow :: (Ord r, Enum r) => r -> Z2 c r a -> Z2 c r a
zipToRow = zipTo

zipToCell :: (Ord c, Ord r, Enum c, Enum r) => (c,r) -> Z2 c r a -> Z2 c r a
zipToCell (c,r) = zipToCol c . zipToRow r

viewCell :: Z2 c r a -> a
viewCell = view . view

window2D :: Int -> Int -> Int -> Int -> Z2 c r a -> [[a]]
window2D u d l r = window u d . fmap (window l r)

indexCell :: Z2 c r a -> (c,r)
indexCell = (index . view &&& index)

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

cell :: (Z2 c r a -> Z2 c r a) -> Z2 c r a -> a
cell = (viewCell .)

cells :: [Z2 c r a -> Z2 c r a] -> Z2 c r a -> [a]
cells fs = map viewCell . (fs <*>) . pure

at :: (Ord c, Enum c, Ord r, Enum r) => (c,r) -> Z2 c r a -> Z2 c r a
at = zipToCell

atRow :: (Ord r, Enum r) => r -> Z2 c r a -> Z2 c r a
atRow = zipToRow

atCol :: (Ord c, Enum c) => c -> Z2 c r a -> Z2 c r a
atCol = zipToCol

aboveBy :: (Ord r, Enum r) => Int -> Z2 c r a -> Z2 c r a
aboveBy r | r > 0     = aboveBy (pred r) . up
aboveBy r | r == 0    = id
aboveBy r | otherwise = belowBy (- r)

belowBy :: (Ord r, Enum r) => Int -> Z2 c r a -> Z2 c r a
belowBy r | r > 0     = belowBy (pred r) . down
belowBy r | r == 0    = id
belowBy r | otherwise = aboveBy (- r)

leftBy :: (Ord c, Enum c) => Int -> Z2 c r a -> Z2 c r a
leftBy r | r > 0     = leftBy (pred r) . left
leftBy r | r == 0    = id
leftBy r | otherwise = rightBy (- r)

rightBy :: (Ord c, Enum c) => Int -> Z2 c r a -> Z2 c r a
rightBy r | r > 0     = rightBy (pred r) . right
rightBy r | r == 0    = id
rightBy r | otherwise = leftBy (- r)

metaZipper2D :: (Enum c, Enum r) => Z2 c r a -> Z2 c r (Z2 c r a)
metaZipper2D = fmap metaZipperVert . metaZipper
   where metaZipperVert = zipIterate left right <$> index . view <*> id

zipLoeb2D :: (Enum c, Enum r, Ord c, Ord r) => Z2 c r (Z2 c r a -> a) -> Z2 c r a
zipLoeb2D fs = fix $ (fmap (<*>) fs <*>) . metaZipper2D

-- Some example zippers for testing...
preview2D = window2D 2 2 2 2

ones :: Z1 Integer Integer
ones = zipperOf 0 1

numberLine :: Z1 Integer Integer
numberLine = zipper 0 (map negate [1..]) 0 [1..]

numberLine2D :: Z2 Integer Integer Integer
numberLine2D = zipper 0 (tail (iterate (fmap pred) numberLine))
                        numberLine
                        (tail (iterate (fmap succ) numberLine))

-- unDigits 26 . map (subtract (pred (fromEnum 'A')) . fromEnum) $ "AB"
-- map (chr . (+ (pred (fromEnum 'A')))) . digits 26
-- (all $ (&&) <$> ('A' <=) <*> (<= 'Z')) "ABC"

fibs :: [[Z2 Int Int Integer -> Integer]]
fibs = ([0,1]                     ++ repeat (cell (leftBy 1) + cell (leftBy 2))) : repeat
       ([0, cell (aboveBy 1) + 1] ++ repeat (cell (leftBy 1) + cell (leftBy 2)))

zeroSheet :: Z2 Int Int Integer
zeroSheet = zipperOf 0 (zipperOf 0 0)

fibsZipper :: Z2 Int Int (Z2 Int Int Integer -> Integer)
fibsZipper = fmap (listToZipper 0 0) . listToZipper 0 0 $ fibs

listToZipper :: i -> a -> [a] -> Z1 i a
listToZipper i def = fmap head . (zipIterate (const [def]) tail i)
             
