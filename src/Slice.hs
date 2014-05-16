{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE UndecidableInstances   #-}

module Slice where

import Tape
import Indexed
import Peano
import Nested
import Reference
import TaggedList
import CountedList ( Pad(..) )

import Stream ( Stream(..) , (<:>) )
import qualified Stream as S

import Control.Applicative
import Prelude hiding ( take )

tapeTake :: Ref Relative -> Tape a -> [a]
tapeTake (Rel r) t | r > 0 = focus t : S.take      r  (viewR t)
tapeTake (Rel r) t | r < 0 = focus t : S.take (abs r) (viewL t)
tapeTake _ _ = []

class Take r t where
   type ListFrom t a
   take :: RefList r -> t a -> ListFrom t a

instance Take Nil (Nested (Flat Tape)) where
   type ListFrom (Nested (Flat Tape)) a = [a]
   take _ _ = []

instance (Take Nil (Nested ts), Functor (Nested ts)) => Take Nil (Nested (Nest ts Tape)) where
   type ListFrom (Nested (Nest ts Tape)) a = ListFrom (Nested ts) [a]
   take _ = take (Rel 0 :-: TNil)

instance Take (Relative :-: Nil) (Nested (Flat Tape)) where
   type ListFrom (Nested (Flat Tape)) a = [a]
   take (r :-: _) (Flat t) = tapeTake r t

instance ( Functor (Nested ts), Take rs (Nested ts) )
         => Take (Relative :-: rs) (Nested (Nest ts Tape)) where
   type ListFrom (Nested (Nest ts Tape)) a = ListFrom (Nested ts) [a]
   take (r :-: rs) (Nest t) = take rs . fmap (tapeTake r) $ t

instance ( Take (Replicate (NestedCount ts) Relative) (Nested ts)
         , Pad (NestedCount ts) (Length r))
         => Take r (Indexed ts) where
   type ListFrom (Indexed ts) a = ListFrom (Nested ts) a
   take r (Indexed i t) = take (heterogenize id (getMovement r i)) t

tapeView :: Ref Relative -> Tape a -> Stream a
tapeView (Rel r) t | r >= 0    = focus t <:> viewR t
tapeView (Rel r) t | otherwise = focus t <:> viewL t

class View r t where
   type StreamFrom t a
   view :: RefList r -> t a -> StreamFrom t a

instance View Nil (Nested (Flat Tape)) where
   type StreamFrom (Nested (Flat Tape)) a = Stream a
   view _ (Flat t) = tapeView (Rel 0) t

instance (View Nil (Nested ts), Functor (Nested ts)) => View Nil (Nested (Nest ts Tape)) where
   type StreamFrom (Nested (Nest ts Tape)) a = StreamFrom (Nested ts) (Stream a)
   view _ = view (Rel 0 :-: TNil)

instance View (Relative :-: Nil) (Nested (Flat Tape)) where
   type StreamFrom (Nested (Flat Tape)) a = (Stream a)
   view (r :-: _) (Flat t) = tapeView r t

instance ( Functor (Nested ts), View rs (Nested ts) )
         => View (Relative :-: rs) (Nested (Nest ts Tape)) where
   type StreamFrom (Nested (Nest ts Tape)) a = StreamFrom (Nested ts) (Stream a)
   view (r :-: rs) (Nest t) = view rs . fmap (tapeView r) $ t

instance ( View (Replicate (NestedCount ts) Relative) (Nested ts)
         , Pad (NestedCount ts) (Length r))
         => View r (Indexed ts) where
   type StreamFrom (Indexed ts) a = StreamFrom (Nested ts) a
   view r (Indexed i t) = view (heterogenize id (getMovement r i)) t

tapeGo :: Ref Relative -> Tape a -> Tape a
tapeGo (Rel r) = fpow (abs r) (if r > 0 then moveR else moveL)
   where fpow n = foldr (.) id . replicate n -- iterate a function n times

class Go r t where
   go :: RefList r -> t a -> t a

instance Go (Relative :-: Nil) (Nested (Flat Tape)) where
   go (r :-: _) (Flat t) = Flat $ tapeGo r t

instance Go Nil (Nested ts) where go _ = id

instance (Go rs (Nested ts), Functor (Nested ts)) => Go (Relative :-: rs) (Nested (Nest ts Tape)) where
   go (r :-: rs) (Nest t) = Nest . go rs . fmap (tapeGo r) $ t

instance ( Go (Replicate (NestedCount ts) Relative) (Nested ts)
         , Pad (NestedCount ts) (Length r)
         , ReifyNatural (NestedCount ts) )
         => Go r (Indexed ts) where
   go r (Indexed i t) =
      let move = getMovement r i
      in  Indexed (merge move i) (go (heterogenize id move) t)

slice :: (Take r' t, Go r t) => RefList r -> RefList r' -> t a -> ListFrom t a
slice r r' = take r' . go r
