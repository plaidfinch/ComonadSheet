{-# LANGUAGE TupleSections #-}

module Checked 
   ( CellExpr(DynamicCell)
   , cell , cells , dcell , dcells
   ) where

import Generic
import qualified Unchecked as U
import PlaneZipper (Z2)
import qualified PlaneZipper as Z2

import Data.Set (Set)
import qualified Data.Set as Set

data CellExpr z ref b = StaticCell  { getRefs :: Set ref, appSCell :: z -> b }
                      | DynamicCell {                     appDCell :: z -> b }

appCell :: CellExpr z r b -> z -> b
appCell (StaticCell _ f) = f
appCell (DynamicCell  f) = f

instance (Show r) => Show (CellExpr z r b) where
   show (StaticCell refs _) = "StaticCell (" ++ (show refs) ++ ") _"
   show (DynamicCell _)     = "DynamicCell _"

instance Functor (CellExpr z r) where
   fmap f (StaticCell refs a) = StaticCell refs (f . a)
   fmap f (DynamicCell a)     = DynamicCell     (f . a)

instance (Ord ref, AnyRef ref z) => Applicative (CellExpr z ref) where
   pure                              = StaticCell Set.empty . const
   StaticCell r a <*> StaticCell s b = StaticCell (r <> s) (a <*> b)
   StaticCell _ a <*> DynamicCell  b = DynamicCell         (a <*> b)
   DynamicCell  a <*> StaticCell _ b = DynamicCell         (a <*> b)
   DynamicCell  a <*> DynamicCell  b = DynamicCell         (a <*> b)

indexDeref :: (Ord x, Enum x) => Ref x -> x -> x
indexDeref = genericDeref pred succ id

cell :: (Ord ref, AnyRef ref z, AnyZipper z i a) => ref -> CellExpr z ref a
cell = StaticCell <$> Set.singleton <*> U.cell

cells :: (Ord ref, AnyRef ref z, AnyZipper z i a) => [ref] -> CellExpr z ref [a]
cells = StaticCell <$> Set.fromList <*> sequence . map (appCell . cell)

dcell :: (z -> a) -> CellExpr z ref a
dcell = DynamicCell

dcells :: (z -> [a]) -> CellExpr z ref [a]
dcells = DynamicCell
