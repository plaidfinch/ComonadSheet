ZipperSheets
============

Spreadsheet evaluation with absolute and relative references can be implemented as the fixed-point of a two-dimensional indexed list zipper.

This work was inspired by reading [this article](http://blog.emillon.org/posts/2012-10-18-comonadic-life.html) and [this one](http://blog.sigfpe.com/2006/11/from-l-theorem-to-spreadsheet.html) on the same day.

Examples
--------

An infinite spreadsheet listing the rows of Pascal's triangle as upwards-rightwards diagonals, from which we can easily extract the rows of the triangle by a simple traversal:

```Haskell
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
         iterate (go $ above & right) .
         goto (0,n) $ pascal
```

```Haskell
> rectangle (0,0) (10,10) pascal
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

An infinite spreadsheet listing Fibonacci-like sequences which have a successively higher second initial parameter:

```Haskell
fibLike :: Z2 Integer Integer Integer
fibLike = evaluate $ sheetOf 0 (0,0) $
           ([1, 1]              ++ fibRow) :
    repeat ([1, 1 + cell above] ++ fibRow)
    where fibRow = repeat $ cell (leftBy 1) + cell (leftBy 2)
```

```Haskell
> rectangle (0,0) (10,10) fibLike
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
