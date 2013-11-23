module PlaneZipper where

import ListZipper

import Control.Applicative
import Control.Arrow hiding (left, right)

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

zipToCell :: (Ord c, Ord r, Enum c, Enum r) => (r,c) -> Z2 c r a -> Z2 c r a
zipToCell (r,c) = zipToCol c . zipToRow r

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

writeRow :: Z1 c a -> Z2 c r a -> Z2 c r a
writeRow = write

insertRowD :: Z1 c a -> Z2 c r a -> Z2 c r a
insertRowD = insertR

insertRowU :: Z1 c a -> Z2 c r a -> Z2 c r a
insertRowU = insertL

deleteRowD :: Z2 c r a -> Z2 c r a
deleteRowD = deleteR

deleteRowU :: Z2 c r a -> Z2 c r a
deleteRowU = deleteL

writeCol :: (Ord r, Enum r) => Z1 r a -> Z2 c r a -> Z2 c r a
writeCol c plane = write <$> c <*> plane

insertColR :: (Ord r, Enum r) => Z1 r a -> Z2 c r a -> Z2 c r a
insertColR c plane = insertR <$> c <*> plane

insertColL :: (Ord r, Enum r) => Z1 r a -> Z2 c r a -> Z2 c r a
insertColL c plane = insertL <$> c <*> plane

deleteColL :: (Ord r, Enum r) => Z2 c r a -> Z2 c r a
deleteColL = fmap deleteL

deleteColR :: (Ord r, Enum r) => Z2 c r a -> Z2 c r a
deleteColR = fmap deleteR

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
