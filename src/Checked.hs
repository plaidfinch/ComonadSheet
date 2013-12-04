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

-- Abs case is actually un-needed and redundant... #TODO
data Ref x = Abs x | Rel Int (Maybe x) deriving (Show, Eq, Ord)

instance Monoid (Ref x) where
   mempty                    = Rel 0 Nothing
   Abs x   `mappend` Rel y z = Rel y       (z <|> Just x)
   Rel w x `mappend` Rel y z = Rel (w + y) (z <|> x)
   _ `mappend` Abs x         = Abs x

at :: (c,r) -> (Ref c,Ref r)
at = (Abs *** Abs)

atRow :: r -> (Ref c,Ref r)
atRow = (mempty,) . Abs

atCol :: c -> (Ref c,Ref r)
atCol = (,mempty) . Abs

aboveBy :: Int -> (Ref c,Ref r)
aboveBy = (mempty,) . flip Rel Nothing . negate

above :: (Ref c,Ref r)
above = aboveBy 1

belowBy :: Int -> (Ref c,Ref r)
belowBy = (mempty,) . flip Rel Nothing

below :: (Ref c,Ref r)
below = belowBy 1

leftBy :: Int -> (Ref c,Ref r)
leftBy = (,mempty) . flip Rel Nothing . negate

left :: (Ref c,Ref r)
left = leftBy 1

rightBy :: Int -> (Ref c,Ref r)
rightBy = (,mempty) . flip Rel Nothing

right :: (Ref c,Ref r)
right = rightBy 1

type CellRef c r = (Ref c,Ref r)

data CellExpr c r a b = CellExpr { getRefs :: Set (CellRef c r)
                                 , appCell  :: Z2 c r a -> b }

instance Functor (CellExpr c r a) where
   fmap f (CellExpr refs a) = CellExpr refs (f . a)

instance (Ord c, Ord r) => Applicative (CellExpr c r a) where
   pure                          = CellExpr Set.empty . const
   CellExpr r a <*> CellExpr s b = CellExpr (r <> s) (a <*> b)

genericDeref :: (x -> a -> a) -> (Int -> a -> a) -> Ref x -> a -> a
genericDeref absolute _        (Abs x)          = absolute x
genericDeref absolute relative (Rel i (Just x)) = relative i . absolute x
genericDeref _        relative (Rel i Nothing)  = relative i

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
cell ref@(c,r) = CellExpr (Set.singleton ref) $ U.cell $ derefCol c . derefRow r

cells :: (Ord c, Ord r, Enum c, Enum r) => [CellRef c r] -> CellExpr c r a [a]
cells refs = CellExpr (Set.fromList refs) $ sequence $ map (appCell . cell) refs


