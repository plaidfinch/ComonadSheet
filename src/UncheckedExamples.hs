module UncheckedExamples where

import Generic
import ListZipper
import PlaneZipper
import Unchecked

import Control.Arrow (first, second)
import Control.Applicative

-- Some example zippers for testing...

fibLike :: Z2 Integer Integer Integer
fibLike = evaluate $ sheetOf 0 $
           ([1, 1]              ++ fibRow) :
    repeat ([1, 1 + cell above] ++ fibRow)
    where fibRow = repeat $ cell (leftBy 1) + cell (leftBy 2)

pascal :: Z2 Integer Integer Integer
pascal = evaluate $ sheetOf 0 $
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
