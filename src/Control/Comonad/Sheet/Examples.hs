module Control.Comonad.Sheet.Examples where

import Control.Comonad.Sheet

import Control.Applicative ( (<$>), (<*>) )
import Data.List ( intersperse )
import Data.Bool ( bool )

import Data.Stream ( Stream , repeat , (<:>) )

import qualified Prelude as P
import Prelude hiding ( repeat , take )

pascal :: Tape2 Integer
pascal = evaluate . sheet 0 $
  repeat 1 <:> repeat (1 <:> pascalRow)
  where pascalRow = repeat $ cell above + cell left

diagonalize :: Tape2 a -> [[a]]
diagonalize = 
   zipWith P.take [1..]
   . map (map extract . P.iterate (go (above & right)))
   . P.iterate (go below)

fibLike :: Tape3 Integer
fibLike = evaluate $ sheet 0 $
   fibSheetFrom 1 1 <:> repeat (fibSheetFrom (cell inward + 1) (cell inward))
   where fibSheetFrom a b = (a          <:> b                <:> fibRow) <:>
                     repeat (cell above <:> (1 + cell above) <:> fibRow)
         fibRow = repeat $ cell (leftBy 1) + cell (leftBy 2)

data Cell = X | O deriving ( Eq , Show )
type Universe = Tape3 Cell
type Ruleset = ([Int],[Int]) -- list of numbers of neighbors to trigger
                             -- being born, and staying alive, respectively

life :: Ruleset -> [[Cell]] -> Universe
life ruleset seed = evaluate $ insert [map (map const) seed] blank
   where blank = sheet (const X) (repeat . tapeOf . tapeOf $ rule)
         rule place  = case (neighbors place `elem`) `onBoth` ruleset of
                            (True,_)  -> O
                            (_,True)  -> cell inward place
                            _         -> X
         neighbors   = length . filter (O ==) . cells bordering
         bordering   = map (inward &) (diagonals ++ verticals ++ horizontals)
         diagonals   = (&) <$> horizontals <*> verticals
         verticals   =        [above, below]
         horizontals = map d2 [right, left]

onBoth :: (a -> b) -> (a,a) -> (b,b)
f `onBoth` (x,y) = (f x,f y)

conway :: [[Cell]] -> Universe
conway = life ([3],[2,3])

printLife :: Int -> Int -> Int -> Universe -> IO ()
printLife c r t = mapM_ putStr
   .            ([separator '┌' '─' '┐'] ++)
   .         (++ [separator '└' '─' '┘']) 
   . intersperse (separator '├' '─' '┤')
   . map (unlines . map (("│ " ++) . (++ " │")) . frame)
   . take (rightBy c & belowBy r & outwardBy t)
   where
      separator x y z = [x] ++ P.replicate (1 + (1 + c) * 2) y ++ [z] ++ "\n"
      frame = map $ intersperse ' ' . map (bool ' ' '●' . (O ==))

glider :: Universe
glider = conway [[X,X,O],
                 [O,X,O],
                 [X,O,O]]

spaceship :: Universe
spaceship = conway [[X,X,X,X,X],
                    [X,O,O,O,O],
                    [O,X,X,X,O],
                    [X,X,X,X,O],
                    [O,X,X,O,X]]
