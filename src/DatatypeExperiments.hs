{-# LANGUAGE GADTs, DataKinds, FlexibleContexts, TypeFamilies #-}

module DatatypeExperiments where

import Prelude hiding (Left,Right)
import Control.Applicative

data ExtentH = InfL | InfLR | InfR
data ExtentV = InfU | InfUD | InfD

infixr 5 :.

data NonEmptyList a = F a | a :. NonEmptyList a deriving Show

infixr 5 :<<:
infixr 5 :>>:

data Row (e :: ExtentH) c r a b where
   (:>>:)  :: [CellExpr c r a b] -> NonEmptyList (CellExpr c r a b) -> Row InfR c r a b
   (:<<:)  :: NonEmptyList (CellExpr c r a b) -> Row InfR c r a b -> Row InfLR c r a b

infixr 5 :\/:
infixr 5 :/\:

data Col (e :: ExtentV) c r a b where
   (:\/:) :: [Row InfLR c r a b] -> NonEmptyList (Row InfLR c r a b) -> Col InfD c r a b
   (:/\:) :: NonEmptyList (Row InfLR c r a b) -> Col InfD c r a b -> Col InfUD c r a b

data CellExpr c r a b = Cell (CellRef c r -> CellRef c r)
                      | App (CellExpr c r a (a -> b)) (CellExpr c r a a)
                      | Con b

data CellRef c r = This
                 | Up          (CellRef c r)
                 | Down        (CellRef c r)
                 | Left        (CellRef c r)
                 | Right       (CellRef c r)
                 | AboveBy Int (CellRef c r)
                 | BelowBy Int (CellRef c r)
                 | LeftBy  Int (CellRef c r)
                 | RightBy Int (CellRef c r)
                 | AtRow   r   (CellRef c r)
                 | AtCol   c   (CellRef c r)
                 | At    (c,r) (CellRef c r)
                 deriving Show

evalCellExpr :: CellExpr c r a b -> a -> b
evalCellExpr (Con a)   = pure a
evalCellExpr (App a z) = (evalCellExpr a) <*> (evalCellExpr z)
--evalCellExpr (Cell r)  = cell up

instance Functor (CellExpr c r a) where
   fmap f (Con b)   = Con (f b)
   fmap f (App a z) = App (fmap (f .) a) z

infixl 4 <&>

(<&>) :: CellExpr c r a (a -> b) -> CellExpr c r a a -> CellExpr c r a b
(<&>) = App

--data FunList a b = Fun b | Arg a (FunList a (a -> b))

--getB :: FunList a b -> b
--getB (Fun b)   = b
--getB (Arg a z) = getB z a

--getAs :: FunList a b -> [a] 
--getAs (Fun _)   = []
--getAs (Arg a z) = a : getAs z

--

x = F (F (Cell Down) :<<: [] :>>: F (Cell Right))
    :/\:
     []
    :\/:
    F (F (Cell Right) :<<: [] :>>: F (Cell Down))
