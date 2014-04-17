{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}

module Tape where

import Control.Comonad
import Control.Arrow
import Control.Applicative
import Data.Distributive
import Data.Functor.Compose

import Stream ( Stream(..) )
import qualified Stream as S
import Prelude hiding ( iterate )

-- | A Tape is like a Turing-machine tape: infinite in both directions, with a focus in the middle.
data Tape a = Tape (Stream a) a (Stream a) deriving ( Functor )

unfold :: (c -> (a,c)) -> (c -> a) -> (c -> (a,c)) -> c -> Tape a
unfold prev center next =
   Tape <$> S.unfold prev <*> center <*> S.unfold next

iterate :: (a -> a) -> (a -> a) -> a -> Tape a
iterate prev next =
   unfold (dup . prev) id (dup . next)
   where dup a = (a,a)

enumerate :: (Enum a) => a -> Tape a
enumerate = iterate pred succ

instance Comonad Tape where
   extract (Tape _ c _) = c
   duplicate = iterate zipL zipR

instance ComonadApply Tape where
   (Tape ls c rs) <@> (Tape ls' c' rs') =
      Tape (ls <@> ls') (c c') (rs <@> rs')

instance Applicative Tape where
   (<*>) = (<@>)
   pure  = Tape <$> pure <*> id <*> pure

instance Distributive Tape where
   distribute =
      unfold (fmap extract &&& fmap zipL)
             (fmap extract)
             (fmap extract &&& fmap zipR)

-- Now let's get multi-dimensional!

type Tape2 = Compose Tape  Tape
type Tape3 = Compose Tape2 Tape
type Tape4 = Compose Tape3 Tape

class Dimension1 z where
  zipL :: z a -> z a
  zipR :: z a -> z a

class Dimension2 z where
  zipU :: z a -> z a
  zipD :: z a -> z a

class Dimension3 z where
  zipI :: z a -> z a
  zipO :: z a -> z a

class Dimension4 z where
  zipA :: z a -> z a
  zipK :: z a -> z a

-- | Apply a function to the inside of a Compose.
composedly :: (f (g a) -> f' (g' a')) -> Compose f g a -> Compose f' g' a'
composedly f = Compose . f . getCompose

instance Dimension1 Tape where
  zipR (Tape ls c (Cons r rs)) = Tape (Cons c ls) r rs
  zipL (Tape (Cons l ls) c rs) = Tape ls l (Cons c rs)

instance (Functor f) => Dimension1 (Compose f Tape) where
  zipL = composedly (fmap zipL)
  zipR = composedly (fmap zipR)

instance (Dimension1 z) => Dimension2 (Compose z Tape) where
  zipU = composedly zipL
  zipD = composedly zipR

instance (Dimension2 z) => Dimension3 (Compose z Tape) where
  zipI = composedly zipU
  zipO = composedly zipD

instance (Dimension3 z) => Dimension4 (Compose z Tape) where
  zipA = composedly zipI
  zipK = composedly zipO

-- | This is just like the Applicative instance given for Compose, but it has the additional
-- Distributive constraint because that's necessary for the whole thing to be a comonad.
instance (ComonadApply f, ComonadApply g, Distributive g) => ComonadApply (Compose f g) where
   Compose f <@> Compose x =
      Compose ((<@>) <$> f <@> x)

-- | Given a (Compose f g) where f and g are comonads, we can define one sensible Comonad instance
-- for the whole. But! It requires that g be Distributive in addition, which enables us to permute
-- the layerings of comonads to produce the proper result.
instance (Comonad f, Comonad g, Distributive g) => Comonad (Compose f g) where
   extract   = extract . extract . getCompose
   duplicate = fmap Compose . Compose -- wrap it again: f (g (f (g a))) -> Compose f g (Compose f g a)
             . fmap distribute        -- swap middle two layers: f (f (g (g a))) -> f (g (f (g a)))
             . duplicate              -- duplicate outer functor f: f (g (g a)) -> f (f (g (g a)))
             . fmap duplicate         -- duplicate inner functor g: f (g a) -> f (g (g a))
             . getCompose             -- unwrap it: Compose f g a -> f (g a)

-- | Cartesian product space for two Tapes.
cross :: Tape a -> Tape b -> Tape2 (a,b)
cross a b = (,) <$> Compose (     pure a)
                <*> Compose (fmap pure b)

-- | Cartesian product space for three Tapes.
cross3 :: Tape a -> Tape b -> Tape c -> Tape3 (a,b,c)
cross3 a b c = (,,) <$> (Compose . Compose) (     pure .      pure $ a)
                    <*> (Compose . Compose) (     pure . fmap pure $ b)
                    <*> (Compose . Compose) (fmap pure . fmap pure $ c)

-- | Cartesian product space for four Tapes.
cross4 :: Tape a -> Tape b -> Tape c -> Tape d -> Tape4 (a,b,c,d)
cross4 a b c d = (,,,) <$> (Compose . Compose . Compose) (     pure .      pure .      pure $ a)
                       <*> (Compose . Compose . Compose) (     pure .      pure . fmap pure $ b)
                       <*> (Compose . Compose . Compose) (     pure . fmap pure . fmap pure $ c)
                       <*> (Compose . Compose . Compose) (fmap pure . fmap pure . fmap pure $ d)

-- Some example zippers for testing...
ints :: Tape Int
ints = enumerate 0
