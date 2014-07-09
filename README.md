ComonadSheet
============

A library for expressing "spreadsheet-like" computations with absolute and relative references, using fixed-points of n-dimensional comonads. A sheet is an n-dimensionally nested `Tape`, which is a stream infinite in both left and right directions, with a focus element. For instance, `type Sheet1 a = Nested (Flat Tape) a`, which is isomorphic to `Tape a`. Nested `Tape`s describe multi-dimensional grid-like spaces, which I will refer to, rather leadingly, as *sheets* made up of *cells*.

While a conventional spreadsheet combines the construction and evaluation of a space of formulae into one process for the user, these steps are distinct in the `ComonadSheet` library. To create a self-referencing spreadsheet-like computation, first construct a multi-dimensional space of functions which take as input a *space of values* and return a *single value*. Then, take its fixed point using the `evaluate` function, resulting in a *space of values*. A type speaks more than a thousand words:

```Haskell
evaluate :: (ComonadApply w) => w (w a -> a) -> w a
```

But if you want a thousand words, you can read the documentation (below and in the source), or listen to me talk:

- ["Getting a Quick Fix on Comonads"](http://vimeo.com/100176795): tech talk at Galois, Inc. on July 1, 2014

Installation
------------

```
$ cabal update
$ cabal install ComonadSheet
```

Creating Sheets
---------------

Usually, the best way to create a sheet is using the `sheet` function, or using the `pure` method of the `Applicative` interface. The `sheet` function takes a default element value, and a structure containing more values, and inserts those values into a space initially filled with the default value. For instance, `sheet 0 [[1]] :: Sheet2 Int` makes a two-dimensional sheet which is 0 everywhere except the focus, which is 1. Note that because of overloading on `sheet`'s operands, it is usually necessary to give a type signature somewhere. This is generally not a problem because GHC can almost always infer the type you wanted if you give it so much as a top-level signature.

References and Manipulation
---------------------------

References to sheets are represented as quasi-heterogeneous lists of absolute and relative references. (In the `Names` module, I've provided names for referring to dimensions up to 4.) A reference which talks about some dimension *n* can be used to refer to that same relative or absolute location in any sheet of dimension *n* or greater.

For instance, `rightBy 5` is a relative reference in the first dimension. If I let `x = sheet 0 [1..] :: Sheet1 Int`, then `extract (go (rightBy 5) x) == 6`. Notice that I used the `extract` method from the sheet's `Comonad` instance to pull out the focus element. Another way to express the same thing would be to say `cell (rightBy 5) x` -- the `cell` function is the composition of `extract` and `go`. In addition to moving around in sheets, I can use references to slice out pieces of them. For instance, `take (rightBy 5) x == [1,2,3,4,5,6]`. (Note that references used in extracting ranges are treated as inclusive.) I can also use a reference to point in a direction and extract an infinite stream (or stream- of-stream-of- streams...) pointed in that direction. For instance, `view right x == [1..]`.

References can be relative or absolute. An absolute reference can only be used to refer to an `Indexed` sheet, as this is the only kind of sheet with a notion of absolute position.

References can be combined using the `(&)` operator. For example, `columnAt 5 & aboveBy 10` represents a reference to a location above the current focus position by 10 cells, and at column 5, regardless of the current column position. Relative references may be combined with one another, and absolute and relative references may be combined, but combining two absolute references is a type error.

Examples
--------

The environment I'll be using as a demo-space looks like:
```Haskell
import Control.Comonad.Sheet
import Data.Stream ( Stream , repeat , (<:>) )

import Control.Applicative ( (<$>), (<*>) )
import Data.List ( intersperse )
import Data.Bool ( bool )

import qualified Prelude as P
import Prelude hiding ( repeat , take )
```

### Iterated Numbers

A one-dimensional sheet which is zero left of the origin and lists the natural numbers right of the origin:

```Haskell
naturals :: Sheet1 Integer
naturals = evaluate $ sheet 0 (repeat (cell left + 1))
```

When we print this out...

```Haskell
> take (rightBy 10) naturals
[1,2,3,4,5,6,7,8,9,10,11]
```

### Pascal's Triangle

An infinite spreadsheet listing the rows of Pascal's triangle as upwards-rightwards diagonals:

```Haskell
pascal :: Sheet2 Integer
pascal = evaluate . sheet 0 $
   repeat 1 <:> repeat (1 <:> pascalRow)
   where pascalRow = repeat $ cell above + cell left
```

Notice the fact that I'm using the `(+)` function to add *functions* (namely, `cell above` and `cell left`). This is thanks to some clever overloading from `Data.Numeric.Function`.

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

We can also traverse it to find the rows of Pascal's triangle, by defining a function to diagonalize an infinite space:

```Haskell
diagonalize :: Sheet2 a -> [[a]]
diagonalize = 
   zipWith P.take [1..]
   . map (map extract . P.iterate (go (above & right)))
   . P.iterate (go below)
```

On Pascal's triangle, this results in:

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

We can define a three-dimensional space enumerating all the Fibonacci-like sequences starting from positive seed numbers a and b, and subsequent terms equal to the sum of the two previous terms. (The normal Fibonacci sequence can be recovered with seeds a = 1, b = 1.)

This example is thanks to an enlightening conversation with Eden Zik.

```Haskell
fibLike :: Sheet3 Integer
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
type Universe = Sheet3 Cell
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

