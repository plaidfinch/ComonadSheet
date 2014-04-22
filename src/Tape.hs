{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleInstances      #-}

module Tape where

import Control.Comonad
import Control.Arrow
import Control.Applicative
import Data.Distributive
import Data.Traversable

import Stream ( Stream(..) )
import qualified Stream as S

import Composition

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

dup :: a -> (a,a)
dup    a =  (a,a)

-- | Given an enumerable type, produce the @Tape@ where the left side is the sequence of predecessors,
--   and the right side is the sequence of successors.
enumerate :: (Enum a) => a -> Tape a
enumerate = iterate pred succ

-- | Tapes form a comonad, where extract gives the focus element and duplicate gives a /diagonalized/ 
--   @Tape (Tape a)@ such that @extract . extract . moveL . duplicate == extract . moveL@ and likewise
--   for @moveR@. 
instance Comonad Tape where
   extract (Tape _ c _) = c
   duplicate = iterate moveL moveR

-- | Applying one tape to another moves them together. This is like the @Applicative@ instances for
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
--   sending movement commands through the functor via @fmap moveL@ and @fmap moveR@, and using
--   @fmap focus@ to remove the extra structure inside the functor. As stated in the Distributive
--   documentation, this can only work if all Tapes have the same cardinality of holes, and if there
--   is no extra information to propagate from outside the functor -- hence, an @Indexed@ tape can't
--   be made into a @Distributive@, as there's no way to extract the index from the functor.
instance Distributive Tape where
   distribute =
      unfold (fmap (focus . moveL) &&& fmap moveL)
             (fmap focus)
             (fmap (focus . moveR) &&& fmap moveR)

moveL, moveR :: Tape a -> Tape a
moveL (Tape (Cons l ls) c rs) = Tape ls l (Cons c rs)
moveR (Tape ls c (Cons r rs)) = Tape (Cons c ls) r rs

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

-- | The tape of integers, with zero centered.
ints :: Tape Integer
ints = enumerate 0

-- | The tape of zeros in all directions
zeros :: Tape Integer
zeros = pure 0
