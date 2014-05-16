module Examples where

import All

import Control.Applicative
import Prelude hiding ( repeat , take )
import qualified Prelude as P

import Stream ( Stream , repeat , (<:>) )

pascal :: Tape2 Integer
pascal = evaluate . sheet 0 $
  repeat 1 <:> repeat (1 <:> pascalRow)
  where pascalRow = repeat $ cell above + cell left

--diagonalize2 :: (Go (Relative :-: Relative :-: Nil) w, Comonad w) => w a -> [[a]]
diagonalize2 :: Tape2 a -> [[a]]
diagonalize2 = 
   zipWith P.take [1..]
   . map (map extract . P.iterate (go (above & right)))
   . P.iterate (go below)

fibLike :: Tape3 Integer
fibLike = evaluate $ sheet 0 $
   fibSheetFrom 1 1 <:> repeat (fibSheetFrom (cell inward + 1) (cell inward))
   where fibSheetFrom a b = (a          <:> b                <:> fibRow) <:> repeat
                            (cell above <:> (1 + cell above) <:> fibRow)
         fibRow = repeat $ cell (leftBy 1) + cell (leftBy 2)

data ConwayCell = X | O deriving ( Eq , Show )
type ConwayUniverse = Tape3 ConwayCell

conway :: [[ConwayCell]] -> ConwayUniverse
conway seed = evaluate $ insert [map (map const) seed] blank
   where blank = sheet (const X) (repeat . tapeOf . tapeOf $ rule)
            where rule z = case neighbors z of
                              2 -> cell inward z
                              3 -> O
                              _ -> X
                  neighbors   = length . filter (== O) <$> cells bordering
                  bordering   = map (inward &) (diagonals ++ verticals ++ horizontals)
                  diagonals   = (&) <$> horizontals <*> verticals
                  verticals   =        [above , below]
                  horizontals = map d2 [left  , right]

printConway :: (Int,Int) -> Int -> ConwayUniverse -> IO ()
printConway (c,r) generations universe =
   (separator >>) . mapM_ ((>> separator) . printFrame) $
      take (rightBy c & belowBy r & outwardBy generations) universe
   where
      separator  = putStrLn $ "+" ++ P.replicate (succ c) '-' ++ "+"
      printFrame = mapM_ $ putStrLn . ("|" ++) . (++ "|") . map showCell
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
