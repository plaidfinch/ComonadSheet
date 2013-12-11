module Unchecked where

import Generic
import ListZipper
import PlaneZipper

import Control.Applicative
import Control.Comonad
import Control.Arrow (first,second)
import Data.Monoid
import Data.Function

evaluate :: (Applicative f, Comonad f) => f (f b -> b) -> f b
evaluate fs = fix $ (fs <*>) . duplicate

cell :: (AnyRef r z, AnyZipper z i a) => r -> z -> a
cell = (view .) . go

cells :: (AnyRef r z, AnyZipper z i a) => [r] -> z -> [a]
cells refs zipper = map (flip cell zipper) refs

genericSheet :: ([Z1 c d] -> Z1 r (Z1 c d) -> Z1 r (Z1 c d))
             -> ([d]      -> Z1 c d   -> Z1 c d)
             -> d -> (a -> d) -> (c,r) -> [[a]] -> Z2 c r d
genericSheet colInsert rowInsert def inject (c,r) =
   Z2 . flip colInsert (zipperOf r (zipperOf c def)) .
   fmap (flip rowInsert $ zipperOf c def) .
   (fmap . fmap $ inject)

sheetOf :: a -> (c,r) -> [[Z2 c r a -> a]] -> Z2 c r (Z2 c r a -> a)
sheetOf def = genericSheet insertListR insertListR (const def) id

-- Some example zippers for testing...

fibLike :: Z2 Integer Integer Integer
fibLike = evaluate $ sheetOf 0 (0,0) $
           ([1, 1]              ++ fibRow) :
    repeat ([1, 1 + cell above] ++ fibRow)
    where fibRow = repeat $ cell (leftBy 1) + cell (leftBy 2)

pascal :: Z2 Integer Integer Integer
pascal = evaluate $ sheetOf 0 (0,0) $
  repeat 1 : repeat (1 : pascalRow)
  where pascalRow = repeat $ cell above + cell left

pascalLists :: [[Integer]]
pascalLists = map pascalList [0..]
   where
      pascalList n =
         map view .
         takeWhile ((>= 0) . row) .
         iterate (go $ above <> right) .
         goto (0,n) $ pascal

numberLine :: Z1 Integer Integer
numberLine = zipper 0 (map negate [1..]) 0 [1..]

numberLine2D :: Z2 Integer Integer Integer
numberLine2D = Z2 $ zipper 0 (tail (iterate (fmap pred) numberLine))
                             numberLine
                             (tail (iterate (fmap succ) numberLine))

cartesian :: Z2 Integer Integer (Integer,Integer)
cartesian = Z2 $ zipper 0 (tail (iterate (fmap (second pred)) (fmap (,0) numberLine)))
                          (fmap (,0) numberLine)
                          (tail (iterate (fmap (second succ)) (fmap (,0) numberLine)))

cartesian' :: Z2 Integer Integer (Integer,Integer)
cartesian' = evaluate $ Z2 $ zipper 0
   [(zipperOf 0 (second pred <$> cell below))]
   (zipper 0 [(first pred <$> cell right)] (const (0,0)) [(first succ <$> cell left)])
   [(zipperOf 0 (second succ <$> cell above))]
