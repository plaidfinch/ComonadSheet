module Examples where

import All hiding ( replicate )

import Control.Applicative
import Prelude hiding ( repeat , take )

import Stream ( Stream , repeat , (<:>) )
import Cartesian

pascal :: Tape2 Integer
pascal = evaluate . sheet 0 $
  repeat 1 <:> repeat (1 <:> pascalRow)
  where pascalRow = repeat $ cell above + cell left

fibLike :: Tape3 Integer
fibLike = evaluate $ sheet 0 $
   fibSheetFrom 1 1 <:> repeat (fibSheetFrom (cell inward + 1) (cell inward))
   where fibSheetFrom a b = (a          <:> b                <:> fibRow) <:> repeat
                            (cell above <:> (1 + cell above) <:> fibRow)
         fibRow = repeat $ cell (leftBy 1) + cell (leftBy 2)

data ConwayCell = X | O deriving ( Eq , Show )
type ConwayUniverse = ITape3 ConwayCell

conway :: [[ConwayCell]] -> ConwayUniverse
conway seed = evaluate $ insert [map (map const) seed] blankConway
   where blankConway = indexedSheet (Abs 0 ::: Abs 0 ::: Abs 0 ::: CNil) (const X) $
            (pure (pure (pure rule)) :: Stream (Tape (Tape (ITape3 ConwayCell -> ConwayCell))))
            where rule z = case neighbors z of
                              2 -> cell inward z
                              3 -> O
                              _ -> X
                  neighbors = length . filter (== O) <$> cells bordering
                  bordering = map (inward &) (diagonals ++ verticals ++ horizontals)
                  verticals   =               [above , below]
                  horizontals = (here2 &) <$> [left  , right]
                  diagonals   = (&) <$> horizontals <*> verticals

printConway :: (Int,Int) -> (Int,Int) -> Int -> ConwayUniverse -> IO ()
printConway (c,r) (c',r') generations universe = do
   separator
   mapM_ ((>> separator) . printGen) $
      slice (columnAt c  & rowAt r  & levelAt 0)
            (columnAt c' & rowAt r' & levelAt generations) 
            universe
   where
      separator = putStrLn $ "+" ++ replicate (1 + abs $ c - c') '-' ++ "+"
      printGen = mapM_ $ putStrLn . ("|" ++) . (++ "|") . map showCell
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
