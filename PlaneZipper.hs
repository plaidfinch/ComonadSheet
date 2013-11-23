module PlaneZipper where

import ListZipper

import Control.Applicative
import Control.Arrow

-- 2-D zippers...

type Z2 i j a = Z1 j (Z1 i a)

zipU :: Enum i => Z2 i j a -> Z2 i j a
zipU = fmap zipL

zipD :: Enum i => Z2 i j a -> Z2 i j a
zipD = fmap zipR

zipToRow :: (Ord i, Enum i) => i -> Z2 i j a -> Z2 i j a
zipToRow i z | i < fst (indexCell z) = zipToRow i $! zipU z
zipToRow i z | i > fst (indexCell z) = zipToRow i $! zipD z
zipToRow i z | otherwise             = z

zipToCol :: (Ord j, Enum j) => j -> Z2 i j a -> Z2 i j a
zipToCol = zipTo

zipToCell :: (Ord i, Ord j, Enum i, Enum j) => (i,j) -> Z2 i j a -> Z2 i j a
zipToCell (i,j) = zipToCol j . zipToRow i

viewU :: Enum i => Z2 i j a -> Z1 j [a]
viewU = fmap viewL

viewD :: Enum i => Z2 i j a -> Z1 j [a]
viewD = fmap viewR

viewCell :: Z2 i j a -> a
viewCell = view . view

window2D :: Int -> Int -> Int -> Int -> Z2 i j a -> [[a]]
window2D u d l r = window u d . fmap (window l r)

indexCell :: Z2 i j a -> (i,j)
indexCell = (index . view &&& index)

writeCell :: a -> Z2 i j a -> Z2 i j a
writeCell a = modify (write a)

modifyCell :: (a -> a) -> Z2 i j a -> Z2 i j a
modifyCell f = writeCell <$> f . viewCell <*> id

writeRow :: Z1 i a -> Z2 i j a -> Z2 i j a
writeRow = write

insertRowD :: Z1 i a -> Z2 i j a -> Z2 i j a
insertRowD = insertR

insertRowU :: Z1 i a -> Z2 i j a -> Z2 i j a
insertRowU = insertL

deleteRowD :: Z2 i j a -> Z2 i j a
deleteRowD = deleteR

deleteRowU :: Z2 i j a -> Z2 i j a
deleteRowU = deleteL

writeCol :: (Ord j, Enum j) => Z1 j a -> Z2 i j a -> Z2 i j a
writeCol col plane = write <$> col <*> plane

insertColR :: (Ord j, Enum j) => Z1 j a -> Z2 i j a -> Z2 i j a
insertColR col plane = insertR <$> col <*> plane

insertColL :: (Ord j, Enum j) => Z1 j a -> Z2 i j a -> Z2 i j a
insertColL col plane = insertL <$> col <*> plane

deleteColL :: (Ord j, Enum j) => Z2 i j a -> Z2 i j a
deleteColL = fmap deleteL

deleteColR :: (Ord j, Enum j) => Z2 i j a -> Z2 i j a
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
