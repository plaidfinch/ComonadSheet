module UncheckedExamples where

import Generic
import Z1
import Z2
import Z3
import Z4
import Unchecked
import NumericInstances

import Control.Arrow (first, second)

-- Some example zippers for testing...

fibLike :: Z3 Int Int Int Integer
fibLike = evaluate $ sheet (0,0,0) 0 $
   fibSheetFrom 1 1 : repeat (fibSheetFrom (cell inward + 1) (cell inward))
   where fibSheetFrom a b = ([a, b]                       ++ fibRow) : repeat
                            ([cell above, 1 + cell above] ++ fibRow)
         fibRow = repeat $ cell (leftBy 1) + cell (leftBy 2)

pascal :: Z2 Int Int Integer
pascal = evaluate $ sheet (0,0) 0 $
  repeat 1 : repeat (1 : pascalRow)
  where pascalRow = repeat $ cell above + cell left

pascalLists :: [[Integer]]
pascalLists = map pascalList [0..]
   where
      pascalList n =
         map view .
         takeWhile ((>= 0) . row) .
         iterate (go $ above & right) .
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
   [zipperOf 0 (second pred <$> cell below)]
   (zipper 0 [first pred <$> cell right] (const (0,0)) [first succ <$> cell left])
   [zipperOf 0 (second succ <$> cell above)]

data ConwayCell = X | O deriving (Eq,Ord,Enum,Show)
type ConwayUniverse = Z3 Int Int Int ConwayCell

conway :: [[ConwayCell]] -> ConwayUniverse
conway seed = evaluate $ insert [map (map const) seed] blankConway
   where blankConway = Z3 $ insert (repeat $ pure rule) (fromZ3 $ pure (const X))
            where rule z = case neighborCount z of
                     2 -> cell inward z
                     3 -> O
                     _ -> X
                  neighborCount = length . filter (== O) <$> cells (map (inward &) bordering)
                  bordering = filter (/= here) $ (&) <$> [left,here,right] <*> [above,here,below]

printConway :: (Int,Int) -> (Int,Int) -> Int -> ConwayUniverse -> IO ()
printConway (c,r) (c',r') generations universe = do
   separator
   mapM_ (\gen -> printGen gen >> separator) $
      slice (at (c,r,0)) (at (c',r',generations)) universe
   where
      separator = putStrLn $ replicate (1 + abs $ c - c') '-'
      printGen = mapM_ $ putStrLn . map showCell
      showCell X = ' '
      showCell O = '*'

lonelyGlider :: ConwayUniverse
lonelyGlider = conway [[X,X,O],
                       [O,X,O],
                       [X,O,O]]

lonelySpaceship :: ConwayUniverse
lonelySpaceship = conway [[X,X,X,X,X],
                          [X,O,O,O,O],
                          [O,X,X,X,O],
                          [X,X,X,X,O],
                          [O,X,X,O,X],
                          [X,X,X,X,X]]
