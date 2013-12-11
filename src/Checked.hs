{-# LANGUAGE TupleSections #-}

module Checked 
   ( CellExpr(DynamicCell)
   , at , atRow , atCol
   , aboveBy , above , belowBy , below
   , leftBy , left , rightBy , right
   , cell , cells , dcell , dcells
   ) where

import Generic
import qualified Unchecked as U
import PlaneZipper (Z2)
import qualified PlaneZipper as Z2

import Control.Applicative
import Control.Arrow hiding (left,right)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set

type CellRef c r = (Ref c,Ref r)

data CellExpr c r a b =
     StaticCell { getRefs  :: Set (CellRef c r)
                , appSCell :: Z2 c r a -> b }
  | DynamicCell { appDCell :: Z2 c r a -> b }

appCell :: CellExpr c r a b -> Z2 c r a -> b
appCell (StaticCell _ f) = f
appCell (DynamicCell  f) = f

instance (Show c, Show r) => Show (CellExpr c r a b) where
   show (StaticCell refs _) = "StaticCell (" ++ (show refs) ++ ") _"
   show (DynamicCell _)     = "DynamicCell _"

instance Functor (CellExpr c r a) where
   fmap f (StaticCell refs a) = StaticCell refs (f . a)
   fmap f (DynamicCell a)     = DynamicCell     (f . a)

instance (Ord c, Ord r) => Applicative (CellExpr c r a) where
   pure                              = StaticCell Set.empty . const
   StaticCell r a <*> StaticCell s b = StaticCell (r <> s) (a <*> b)
   StaticCell _ a <*> DynamicCell  b = DynamicCell         (a <*> b)
   DynamicCell  a <*> StaticCell _ b = DynamicCell         (a <*> b)
   DynamicCell  a <*> DynamicCell  b = DynamicCell         (a <*> b)

indexDeref :: (Ord x, Enum x) => Ref x -> x -> x
indexDeref = genericDeref pred succ id

cell :: (Ord r, Enum r, Ord c, Enum c) => CellRef c r -> CellExpr c r a a
cell = StaticCell <$> Set.singleton <*> U.cell

cells :: (Ord r, Enum r, Ord c, Enum c) => [CellRef c r] -> CellExpr c r a [a]
cells = StaticCell <$> Set.fromList <*> sequence . map (appCell . cell)

dcell :: (Ord r, Enum r, Ord c, Enum c) => (Z2 c r a -> a) -> CellExpr c r a a
dcell = DynamicCell

dcells :: (Ord r, Enum r, Ord c, Enum c) => (Z2 c r a -> [a]) -> CellExpr c r a [a]
dcells = DynamicCell
