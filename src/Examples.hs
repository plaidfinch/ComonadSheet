module Examples where

import All

import Control.Applicative
import Prelude hiding ( repeat , take )

import Stream ( Stream , repeat , (<:>) )

pascal :: Nested (Nest (Flat Tape) Tape) Integer
pascal = evaluate . sheet 0 $
  repeat 1 <:> repeat (1 <:> pascalRow)
  where pascalRow = repeat $ cell above + cell left
