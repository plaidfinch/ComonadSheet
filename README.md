ZipperSheets
============

Spreadsheet evaluation with absolute and relative references can be implemented as the fixed-point of a two-dimensional indexed list zipper.

This work was inspired by reading [this article](http://blog.emillon.org/posts/2012-10-18-comonadic-life.html) and [this one](http://blog.sigfpe.com/2006/11/from-l-theorem-to-spreadsheet.html) on the same day.

Examples
--------

An infinite spreadsheet listing the rows of Pascal's triangle as upwards-rightwards diagonals:

```Haskell
pascal :: Z2 Integer Integer Integer
pascal = evaluate $ sheetOf (0,0) 0 $
  repeat 1 : repeat (1 : pascalRow)
  where pascalRow = repeat $ cell above + cell left
```

This looks like:

```Haskell
> slice (at (0,0)) (at (10,10)) pascal
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
pascalLists :: [[Integer]]
pascalLists = map pascalList [0..]
   where
      pascalList n =
         map view .
         takeWhile ((>= 0) . row) .
         iterate (go $ above & right) .
         goto (0,n) $ pascal
```

This results in:

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

We may define a three-dimensional space enumerating all the Fibonacci-like sequences starting from positive seed numbers a and b, and subsequent terms equal to the sum of the two previous terms. (The normal Fibonacci sequence can be recovered with seeds a = 1, b = 1.)

```Haskell
fibLike :: Z3 Integer Integer Integer Integer
fibLike = evaluate $ sheetOf (0,0,0) 0 $
   fibSheetFrom 1 1 : repeat (fibSheetFrom (cell inward + 1) (cell inward))
   where fibSheetFrom a b = (([a, b]                       ++ fibRow) : repeat
                             ([cell above, 1 + cell above] ++ fibRow))
         fibRow = repeat $ cell (leftBy 1) + cell (leftBy 2)
```

Examining a slice of this space, we find the following:

```Haskell
> slice (at (0,0,0)) (at (4,4,4)) fibLike
[[[1,1,2, 3, 5], -- the original Fibonacci sequence
  [1,2,3, 5, 8],
  [1,3,4, 7,11],
  [1,4,5, 9,14],
  [1,5,6,11,17]],
 [[2,1,3, 4, 7],
  [2,2,4, 6,10],
  [2,3,5, 8,13],
  [2,4,6,10,16],
  [2,5,7,12,19]],
 [[3,1,4, 5, 9], -- a curious coincidence with the opening digits of pi
  [3,2,5, 7,12],
  [3,3,6, 9,15],
  [3,4,7,11,18],
  [3,5,8,13,21]],
 [[4,1,5, 6,11],
  [4,2,6, 8,14],
  [4,3,7,10,17],
  [4,4,8,12,20],
  [4,5,9,14,23]],
 [[5,1,6,  7,13],
  [5,2,7,  9,16],
  [5,3,8, 11,19],
  [5,4,9, 13,22],
  [5,5,10,15,25]]]
```
