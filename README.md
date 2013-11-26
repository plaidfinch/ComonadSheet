ZipperSheets
============

Spreadsheet evaluation with absolute and relative references can be implemented as the fixed-point of a two-dimensional indexed list zipper.

Examples
--------

An infinite spreadsheet listing Fibonacci-like sequences which have a successively higher second initial parameter:

```Haskell
fibs :: Z2 Integer Integer Integer
fibs = evaluate2D $ sheetOf 0 (0,0) $
           ([1, 1]                    ++ fibRow) :
    repeat ([1, cell (aboveBy 1) + 1] ++ fibRow)
    where fibRow = repeat $ cell (leftBy 1) + cell (leftBy 2)
```

```Haskell
> rectangle (0,0) (10,10) fibs
[[1,  1,  2,  3,  5,  8, 13,  21,  34,  55], 
 [1,  2,  3,  5,  8, 13, 21,  34,  55,  89], 
 [1,  3,  4,  7, 11, 18, 29,  47,  76, 123], 
 [1,  4,  5,  9, 14, 23, 37,  60,  97, 157], 
 [1,  5,  6, 11, 17, 28, 45,  73, 118, 191], 
 [1,  6,  7, 13, 20, 33, 53,  86, 139, 225], 
 [1,  7,  8, 15, 23, 38, 61,  99, 160, 259], 
 [1,  8,  9, 17, 26, 43, 69, 112, 181, 293], 
 [1,  9, 10, 19, 29, 48, 77, 125, 202, 327], 
 [1, 10, 11, 21, 32, 53, 85, 138, 223, 361]]
```

An infinite spreadsheet listing the rows of Pascal's triangle as upwards-rightwards diagonals, from which we can easily extract the rows of the triangle by a simple traversal:

```Haskell
pascal :: Z2 Integer Integer Integer
pascal = evaluate2D $ sheetOf 0 (0,0) $
  repeat 1 : repeat (1 : pascalRow)
  where pascalRow = repeat $ cell up + cell left

pascalLists :: [[Integer]]
pascalLists = map pascalList [0..]
   where
      pascalList n =
         map viewCell .
         takeWhile ((>= 0) . row) .
         iterate (up . right) .
         at (0,n) $ pascal
```

```Haskell
> take 15 pascalLists
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
