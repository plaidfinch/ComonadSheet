module Unchecked where

import ListZipper
import PlaneZipper

import Control.Applicative
import Control.Comonad
import Data.Function

evaluate2D :: (Enum c, Enum r, Ord c, Ord r) => Z2 c r (Z2 c r a -> a) -> Z2 c r a
evaluate2D fs = fix $ (fmap (<*>) fs <*>) . duplicate2D

-- We can't give a comonad instance for 2D zippers, as they're (currently) implemented as a type synonym. We can, however, define a comonad's duplicate, which is what we need.
duplicate2D :: (Ord c, Enum c, Ord r, Enum r) => Z2 c r a -> Z2 c r (Z2 c r a)
duplicate2D = fmap duplicateVert . duplicate
   where duplicateVert = zipIterate left right <$> index . view <*> id

at :: (Ord c, Enum c, Ord r, Enum r) => (c,r) -> Z2 c r a -> Z2 c r a
at = zipToCell

atRow :: (Ord r, Enum r) => r -> Z2 c r a -> Z2 c r a
atRow = zipToRow

atCol :: (Ord c, Enum c) => c -> Z2 c r a -> Z2 c r a
atCol = zipToCol

aboveBy :: (Ord r, Enum r) => Int -> Z2 c r a -> Z2 c r a
aboveBy r | r > 0     = aboveBy (pred r) . above
aboveBy r | r == 0    = id
aboveBy r | otherwise = belowBy (- r)

belowBy :: (Ord r, Enum r) => Int -> Z2 c r a -> Z2 c r a
belowBy r | r > 0     = belowBy (pred r) . below
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

cell :: (Z2 c r a -> Z2 c r a) -> Z2 c r a -> a
cell = (viewCell .)

cells :: [Z2 c r a -> Z2 c r a] -> Z2 c r a -> [a]
cells fs = map viewCell . (fs <*>) . pure

genericSheet :: ([Z1 c d] -> Z2 c r d -> Z2 c r d)
             -> ([d]      -> Z1 c d   -> Z1 c d)
             -> d -> (a -> d) -> (c,r) -> [[a]] -> Z2 c r d
genericSheet colInsert rowInsert def inject (c,r) =
   flip colInsert (zipperOf r (zipperOf c def)) .
   fmap (flip rowInsert $ zipperOf c def) .
   (fmap . fmap $ inject)

sheetOf :: a -> (c,r) -> [[Z2 c r a -> a]] -> Z2 c r (Z2 c r a -> a)
sheetOf def = genericSheet insertListR insertListR (const def) id

-- Some example zippers for testing...

numberLine :: Z1 Integer Integer
numberLine = zipper 0 (map negate [1..]) 0 [1..]

numberLine2D :: Z2 Integer Integer Integer
numberLine2D = zipper 0 (tail (iterate (fmap pred) numberLine))
                        numberLine
                        (tail (iterate (fmap succ) numberLine))

fibLike :: Z2 Integer Integer Integer
fibLike = evaluate2D $ sheetOf 0 (0,0) $
           ([1, 1]           ++ fibRow) :
    repeat ([1, 1 + cell above] ++ fibRow)
    where fibRow = repeat $ cell (leftBy 1) + cell (leftBy 2)

pascal :: Z2 Integer Integer Integer
pascal = evaluate2D $ sheetOf 0 (0,0) $
  repeat 1 : repeat (1 : pascalRow)
  where pascalRow = repeat $ cell above + cell left

pascalLists :: [[Integer]]
pascalLists = map pascalList [0..]
   where
      pascalList n =
         map viewCell .
         takeWhile ((>= 0) . row) .
         iterate (above . right) .
         at (0,n) $ pascal
