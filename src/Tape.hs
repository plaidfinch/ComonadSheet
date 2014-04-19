{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleInstances      #-}

module Tape where

import Control.Comonad
import Control.Arrow
import Control.Applicative
import Data.Distributive
import Data.Traversable
import Data.Functor.Compose

import Stream ( Stream(..) )
import qualified Stream as S

import Prelude hiding ( iterate , take )

-- | A @Tape@ is like a Turing-machine tape: infinite in both directions, with a focus in the middle.
data Tape a = Tape { viewL :: Stream a -- ^ the side of the @Tape@ left of @focus@
                   , focus :: a        -- ^ the focused element
                   , viewR :: Stream a -- ^ the side of the @Tape@ right of @focus@
                   } deriving ( Functor )

-- | Produce a @Tape@ from a seed value, ala unfoldr for lists, or unfold for @Stream@s.
unfold :: (c -> (a,c)) -- ^ leftwards unfolding function
       -> (c -> a)     -- ^ function giving the focus value from the seed
       -> (c -> (a,c)) -- ^ rightwards unfolding function
       -> c            -- ^ seed value
       -> Tape a
unfold prev center next =
   Tape <$> S.unfold prev <*> center <*> S.unfold next

-- | Produce a @Tape@ consisting of the infinite iteration of two functions to a starting focus value,
--   ala iterate for lists or @Stream@s.
iterate :: (a -> a) -- ^ leftwards iteration function
        -> (a -> a) -- ^ rightwards iteration function
        -> a        -- ^ focus value
        -> Tape a
iterate prev next =
   unfold (dup . prev) id (dup . next)
   where dup a = (a,a)

-- | Given an enumerable type, produce the @Tape@ where the left side is the sequence of predecessors,
--   and the right side is the sequence of successors.
enumerate :: (Enum a) => a -> Tape a
enumerate = iterate pred succ

-- | Tapes form a comonad, where extract gives the focus element and duplicate gives a /diagonalized/ 
--   @Tape (Tape a)@ such that @extract . extract . zipL . duplicate == extract . zipL@ and likewise
--   for @zipR@. 
instance Comonad Tape where
   extract (Tape _ c _) = c
   duplicate = iterate zipL zipR

-- | Applying one tape to another zips them together. This is like the @Applicative@ instances for
--   @ZipList@ and @Stream@.
instance ComonadApply Tape where
   (Tape ls c rs) <@> (Tape ls' c' rs') =
      Tape (ls <@> ls') (c c') (rs <@> rs')

-- | A tape is @Applicative@, where the @\<*\>@ is equivalent to its @ComonadApply@ instance (required
--   by law), and a pure value is the tape consisting of copies of that value in both directions.
instance Applicative Tape where
   (<*>) = (<@>)
   pure  = Tape <$> pure <*> id <*> pure

-- | Tapes are @Distributive@ because we can replicate their structure on the outside of a functor by
--   sending movement commands through the functor via @fmap zipL@ and @fmap zipR@, and using
--   @fmap focus@ to remove the extra structure inside the functor. As stated in the Distributive
--   documentation, this can only work if all Tapes have the same cardinality of holes, and if there
--   is no extra information to propagate from outside the functor -- hence, an @Indexed@ tape can't
--   be made into a @Distributive@, as there's no way to extract the index from the functor.
instance Distributive Tape where
   distribute =
      unfold (fmap focus &&& fmap zipL)
             (fmap focus)
             (fmap focus &&& fmap zipR)

-- Now let's get multi-dimensional!

-- | A @Tape2@ is a 2-dimensional tape formed by the composition of two 1D tapes.
type Tape2 = Compose Tape  Tape
-- | A @Tape3@ is a 3-dimensional tape formed by the composition of a 2D tape and a 1D tape.
type Tape3 = Compose Tape2 Tape
-- | A @Tape4@ is a 4-dimensional tape formed by the composition of a 3D tape and a 1D tape.
type Tape4 = Compose Tape3 Tape

-- | Any tape of one dimension or higher can be zipped /left/ or /right/.
class Dimension1 t where
  -- | Moves /left/ by one step (if it's indexed, this is the negative direction).
  zipL :: t a -> t a 
  -- | Moves /right/ by one step (if it's indexed, this is the positive direction).
  zipR :: t a -> t a 

-- | Any tape of two dimensions or higher can be zipped /up/ or /down/.
class Dimension2 t where
  -- | Moves /up/ by one step (if indexed, negative direction).
  zipU :: t a -> t a 
  -- | Moves /down/ by one step (if indexed, positive direction).
  zipD :: t a -> t a 

-- | Any tape of three dimensions or higher can be zipped /in/ or /out/.
class Dimension3 t where
  -- | Moves /in/ by one step (if indexed, negative direction).
  zipI :: t a -> t a 
  -- | Moves /out/ by one step (if indexed, positive direction).
  zipO :: t a -> t a 

-- | Any tape of four dimensions or higher can be zipped /ana/ or /kata/ (following the nomenclature
--   of <http://en.wikipedia.org/wiki/Charles_Howard_Hinton Charles Howard Hinton>).
class Dimension4 t where
  -- | Moves /ana/ by one step (if indexed, negative direction).
  zipA :: t a -> t a 
  -- | Moves /kata/ by one step (if indexed, positive direction).
  zipK :: t a -> t a 

-- | Apply a function to the inside of a @Compose@.
--
--   @composedly f = Compose . f . getCompose@
composedly :: (f (g a) -> f' (g' a')) -> Compose f g a -> Compose f' g' a'
composedly f = Compose . f . getCompose

-- | A single tape is one-dimensional in the obvious way. This is the base-case instance that allows
--   all the higher-dimensional instances to work properly.
instance Dimension1 Tape where
  zipR (Tape ls c (Cons r rs)) = Tape (Cons c ls) r rs
  zipL (Tape (Cons l ls) c rs) = Tape ls l (Cons c rs)

-- | Telling a nested tape to move in the first axis always moves the inner-most tape, regardless of
--   how deeply nested it is.
instance (Functor f) => Dimension1 (Compose f Tape) where
  zipL = composedly (fmap zipL)
  zipR = composedly (fmap zipR)

-- | Telling a nested tape to move in the second axis always moves the second-inner-most tape,
--   regardless of the depth of nesting.
instance (Dimension1 t) => Dimension2 (Compose t Tape) where
  zipU = composedly zipL
  zipD = composedly zipR

-- | Telling a nested tape to move in the third axis always moves the third-inner-most tape.
instance (Dimension2 t) => Dimension3 (Compose t Tape) where
  zipI = composedly zipU
  zipO = composedly zipD

-- | Telling a nested tape to move in the fourth axis always moves the fourth-inner-most tape.
instance (Dimension3 t) => Dimension4 (Compose t Tape) where
  zipA = composedly zipI
  zipK = composedly zipO

-- | This is just like the @Applicative@ instance given for @Compose@, but it has the additional
--   @Distributive@ constraint because that's necessary for the whole thing to be a comonad.
instance (ComonadApply f, ComonadApply g, Distributive g) => ComonadApply (Compose f g) where
   Compose f <@> Compose x =
      Compose ((<@>) <$> f <@> x)

-- | Given a @Compose f g@ where @f@ and @g@ are comonads, we can define a sensible @Comonad@ instance
--   for the whole. But! It requires that g be @Distributive@ in addition, which enables us to permute
--   the layerings of comonads to produce the proper result.
--
--   This might be an overly-general instance, because it would conflict with another possible
--   (valid) definition for the composition of two comonads, but which requires constraints we can't
--   satisfy with the comonads we're using. Namely, you can also instantiate this instance if you
--   replace @fmap distribute@ with @fmap sequenceA@ (from @Data.Traversable@), and let the resulting
--   instance have the constraints @(Comonad f, Comonad g, Traversable f, Applicative g)@. Since not
--   all of our comonads in this project are applicative, and no infinite structure (e.g. @Tape@) has
--   a good (i.e. not returning bottom in unpredictable ways) definition of @Traversable@, using the
--   @Distributive@ constraint makes much more sense here.
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

-- | The tape of integers, with zero centered.
ints :: Tape Integer
ints = enumerate 0

-- | The tape of zeros in all directions
zeros :: Tape Integer
zeros = pure 0
