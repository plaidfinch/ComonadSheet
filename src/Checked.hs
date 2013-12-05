{-# LANGUAGE TupleSections #-}

module Checked 
   ( Ref(..) , CellRef , CellExpr , cell , cells
   , at , atRow , atCol
   , aboveBy , above
   , belowBy , below
   , leftBy , left
   , rightBy , right
   ) where

import qualified Unchecked as U
import PlaneZipper (Z2)
import qualified PlaneZipper as PZ

import Control.Applicative
import Control.Arrow hiding (left,right)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set

data Ref x = Abs Int x | Rel Int deriving (Show, Eq, Ord)

instance Monoid (Ref x) where
   mempty                    = Rel 0
   Abs x y `mappend` Rel z   = Abs (x + z) y
   Rel x   `mappend` Abs y z = Abs (x + y) z
   Rel x   `mappend` Rel y   = Rel (x + y)
   Abs y z `mappend` Abs _ _ = Abs y z

at :: (c,r) -> (Ref c,Ref r)
at = (Abs 0 *** Abs 0)

atRow :: r -> (Ref c,Ref r)
atRow = (mempty,) . Abs 0

atCol :: c -> (Ref c,Ref r)
atCol = (,mempty) . Abs 0

aboveBy :: Int -> (Ref c,Ref r)
aboveBy = (mempty,) . Rel . negate

above :: (Ref c,Ref r)
above = aboveBy 1

belowBy :: Int -> (Ref c,Ref r)
belowBy = (mempty,) . Rel

below :: (Ref c,Ref r)
below = belowBy 1

leftBy :: Int -> (Ref c,Ref r)
leftBy = (,mempty) . Rel . negate

left :: (Ref c,Ref r)
left = leftBy 1

rightBy :: Int -> (Ref c,Ref r)
rightBy = (,mempty) . Rel

right :: (Ref c,Ref r)
right = rightBy 1

type CellRef c r = (Ref c,Ref r)

data CellExpr c r a b =
   StaticCell  { getRefs :: Set (CellRef c r)
               , appSCell :: Z2 c r a -> b } |
   DynamicCell { appDCell :: Z2 c r a -> b }

instance Functor (CellExpr c r a) where
   fmap f (StaticCell refs a) = StaticCell refs (f . a)
   fmap f (DynamicCell a)     = DynamicCell     (f . a)

instance (Ord c, Ord r) => Applicative (CellExpr c r a) where
   pure                              = StaticCell Set.empty . const
   StaticCell r a <*> StaticCell s b = StaticCell (r <> s) (a <*> b)
   StaticCell _ a <*> DynamicCell  b = DynamicCell         (a <*> b)
   DynamicCell  a <*> StaticCell _ b = DynamicCell         (a <*> b)
   DynamicCell  a <*> DynamicCell  b = DynamicCell         (a <*> b)

genericDeref :: (x -> a -> a) -> (Int -> a -> a) -> Ref x -> a -> a
genericDeref _        relative (Rel i)   = relative i
genericDeref absolute relative (Abs i x) = relative i . absolute x

derefRow :: (Ord r, Enum r) => Ref r -> Z2 c r a -> Z2 c r a
derefRow = genericDeref U.atRow U.belowBy

derefCol :: (Ord c, Enum c) => Ref c -> Z2 c r a -> Z2 c r a
derefCol = genericDeref U.atCol U.rightBy

indexDeref :: Enum x => Ref x -> x -> x
indexDeref = genericDeref const relative
   where
      relative n z | n > 0     = iterate succ z !! n
      relative n z | otherwise = iterate pred z !! (negate n)

cell :: (Ord c, Ord r, Enum c, Enum r) => CellRef c r -> CellExpr c r a a
cell ref@(c,r) = StaticCell (Set.singleton ref) $ U.cell $ derefCol c . derefRow r

cells :: (Ord c, Ord r, Enum c, Enum r) => [CellRef c r] -> CellExpr c r a [a]
cells refs = StaticCell (Set.fromList refs) $ sequence $ map (appSCell . cell) refs


