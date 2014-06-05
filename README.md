ZipperSheets
============

Spreadsheet evaluation with absolute and relative references can be implemented as a comonadic fixed-point of an n-dimensional indexed list zipper.

This work was inspired by reading [this article](http://blog.emillon.org/posts/2012-10-18-comonadic-life.html) and [this one](http://blog.sigfpe.com/2006/11/from-l-theorem-to-spreadsheet.html) on the same day.

Examples
--------

The environment I'll be using as a demo-space looks like:
```Haskell
import All
import Stream ( Stream , repeat , (<:>) )

import Control.Applicative ( (<$>), (<*>) )
import Data.List ( intersperse )
import Data.Bool ( bool )

import qualified Prelude as P
import Prelude hiding ( repeat , take )
```

### Pascal's Triangle

An infinite spreadsheet listing the rows of Pascal's triangle as upwards-rightwards diagonals:

```Haskell
pascal :: Tape2 Integer
pascal = evaluate . sheet 0 $
   repeat 1 <:> repeat (1 <:> pascalRow)
   where pascalRow = repeat $ cell above + cell left
```

This looks like:

```Haskell
> take (belowBy 9 & rightBy 9) pascal
[[1,  1,  1,   1,   1,    1,    1,     1,     1,     1], 
 [1,  2,  3,   4,   5,    6,    7,     8,     9,    10], 
 [1,  3,  6,  10,  15,   21,   28,    36,    45,    55], 
 [1,  4, 10,  20,  35,   56,   84,   120,   165,   220], 
 [1,  5, 15,  35,  70,  126,  210,   330,   495,   715], 
 [1,  6, 21,  56, 126,  252,  462,   792,  1287,  2002], 
 [1,  7, 28,  84, 210,  462,  924,  1716,  3003,  5005], 
 [1,  8, 36, 120, 330,  792, 1716,  3432,  6435, 11440], 
 [1,  9, 45, 165, 495, 1287, 3003,  6435, 12870, 24310], 
 [1, 10, 55, 220, 715, 2002, 5005, 11440, 24310, 48620]]
```

We can also traverse it to find the rows of Pascal's triangle, which are the diagonals of this spreadsheet:

```Haskell
diagonalize :: Tape2 a -> [[a]]
diagonalize = 
   zipWith P.take [1..]
   . map (map extract . P.iterate (go (above & right)))
   . P.iterate (go below)
```

This results in:

```Haskell
> P.take 15 (diagonalize pascal)
[[1],
 [1,  1], 
 [1,  2,  1], 
 [1,  3,  3,   1], 
 [1,  4,  6,   4,    1], 
 [1,  5, 10,  10,    5,    1], 
 [1,  6, 15,  20,   15,    6,    1], 
 [1,  7, 21,  35,   35,   21,    7,    1], 
 [1,  8, 28,  56,   70,   56,   28,    8,    1], 
 [1,  9, 36,  84,  126,  126,   84,   36,    9,    1], 
 [1, 10, 45, 120,  210,  252,  210,  120,   45,   10,    1], 
 [1, 11, 55, 165,  330,  462,  462,  330,  165,   55,   11,   1], 
 [1, 12, 66, 220,  495,  792,  924,  792,  495,  220,   66,  12,  1], 
 [1, 13, 78, 286,  715, 1287, 1716, 1716, 1287,  715,  286,  78, 13,  1], 
 [1, 14, 91, 364, 1001, 2002, 3003, 3432, 3003, 2002, 1001, 364, 91, 14, 1]]
```

### Fibonacci-like Sequences

We may define a three-dimensional space enumerating all the Fibonacci-like sequences starting from positive seed numbers a and b, and subsequent terms equal to the sum of the two previous terms. (The normal Fibonacci sequence can be recovered with seeds a = 1, b = 1.)

This example is thanks to an enlightening conversation with Eden Zik.

```Haskell
fibLike :: Tape3 Integer
fibLike = evaluate $ sheet 0 $
   fibSheetFrom 1 1 <:> repeat (fibSheetFrom (cell inward + 1) (cell inward))
   where fibSheetFrom a b = (a          <:> b                <:> fibRow) <:>
                     repeat (cell above <:> (1 + cell above) <:> fibRow)
         fibRow = repeat $ cell (leftBy 1) + cell (leftBy 2)
```

Examining a slice of this space, we find the following:

```Haskell
> take (rightBy 4 & belowBy 4 & outwardBy 2) fibLike
[[[1,1,2, 3, 5], -- the original Fibonacci sequence
  [1,2,3, 5, 8],
  [1,3,4, 7,11],
  [1,4,5, 9,14],
  [1,5,6,11,17]],
 [[2,1,3, 4, 7],
  [2,2,4, 6,10], -- double the Fibonacci sequence
  [2,3,5, 8,13],
  [2,4,6,10,16],
  [2,5,7,12,19]],
 [[3,1,4, 5, 9], -- a curious coincidence with the opening digits of pi
  [3,2,5, 7,12],
  [3,3,6, 9,15], -- triple the Fibonacci sequence
  [3,4,7,11,18],
  [3,5,8,13,21]]]
```

### Conway's Game of Life

Of course, as this is a comonadic library, we're obligated to implement the canonical nontrivial comonadic computation: Conway's Game of Life.

For convenience, we define a few types:

```Haskell
data Cell = X | O deriving ( Eq , Show )
type Universe = Tape3 Cell
type Ruleset = ([Int],[Int]) -- list of numbers of neighbors to trigger
                             -- being born, and staying alive, respectively
```

Then we can define a function which takes a starting configuration (seed) for the Game of Life, and inserts it into the infinite universe of Game-of-Life cells.

Here, we represent the evolution of an instance of the game of life as a three-dimensional space where two axes are space, and the third is time.

In the Conway space, all cells before time zero are always dead cells, and all cells starting at time zero are equal to the Life rule applied to their neighboring cells in the previous time frame. To instantiate a timeline for a seed pattern, it is inserted as a series of constant cells into time frame zero of the blank Conway space. Then, the Conway space is evaluated, resulting in an infinite 3D space showing the evolution of the pattern.

```Haskell
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
```

For aesthetics, we can define a printer function for generations of the game of life. Note that the printer function is more or less as long as the definition of the real computation!

```Haskell
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
```

Here's how we define a universe containing only a single glider:

```Haskell
glider :: Universe
glider = conway [[X,X,O],
                 [O,X,O],
                 [X,O,O]]
```

And it works!

```
> printLife 3 3 4 glider
┌─────────┐
│     ●   │
│ ●   ●   │
│   ● ●   │
│         │
├─────────┤
│   ●     │
│     ● ● │
│   ● ●   │
│         │
├─────────┤
│     ●   │
│       ● │
│   ● ● ● │
│         │
├─────────┤
│         │
│   ●   ● │
│     ● ● │
│     ●   │
├─────────┤
│         │
│       ● │
│   ●   ● │
│     ● ● │
└─────────┘
```

Here's a Lightweight Spaceship:

```Haskell
spaceship :: Universe
spaceship = conway [[X,X,X,X,X],
                    [X,O,O,O,O],
                    [O,X,X,X,O],
                    [X,X,X,X,O],
                    [O,X,X,O,X]]
```

When we run it...

```
> printLife 6 4 4 spaceship
┌───────────────┐
│               │
│   ● ● ● ●     │
│ ●       ●     │
│         ●     │
│ ●     ●       │
├───────────────┤
│     ● ●       │
│   ● ● ● ●     │
│   ● ●   ● ●   │
│       ● ●     │
│               │
├───────────────┤
│   ●     ●     │
│           ●   │
│   ●       ●   │
│     ● ● ● ●   │
│               │
├───────────────┤
│               │
│         ● ●   │
│     ● ●   ● ● │
│     ● ● ● ●   │
│       ● ●     │
├───────────────┤
│               │
│       ● ● ● ● │
│     ●       ● │
│             ● │
│     ●     ●   │
└───────────────┘
```

