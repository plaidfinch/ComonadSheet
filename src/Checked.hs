{-# LANGUAGE TupleSections #-}

module Safe where

import qualified Unchecked as U
import PlaneZipper

import Prelude hiding (Left, Right)
import Control.Applicative
import Control.Arrow
import Data.Monoid

data Ref x = Abs x | Rel Int (Maybe x) deriving (Show, Eq)

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

belowBy :: Int -> (Ref c,Ref r)
belowBy = (mempty,) . flip Rel Nothing

leftBy :: Int -> (Ref c,Ref r)
leftBy = (,mempty) . flip Rel Nothing . negate

rightBy :: Int -> (Ref c,Ref r)
rightBy = (,mempty) . flip Rel Nothing

type CellRef c r = (Ref c,Ref r)

data CellExpr c r a b = CellExpr { cellRefs :: [CellRef c r]
                                 , appCell  :: (Z2 c r a -> b) }

instance Functor (CellExpr c r a) where
   fmap f (CellExpr refs a) = CellExpr refs (f . a)

instance Applicative (CellExpr c r a) where
   pure                          = CellExpr [] . const
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

cell :: (Ord c, Ord r, Enum c, Enum r) => (Ref c,Ref r) -> CellExpr c r a a
cell ref@(c,r) = CellExpr (pure ref) $ U.cell $ derefCol c . derefRow r
