{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Z3
   ( module Generic , Z3(..)
   ) where

import Generic
import Z1
import Z2

newtype Z3 c r l a = Z3 { fromZ3 :: Z1 l (Z2 c r a) }

wrapZ3 :: (Z1 l (Z2 c r a) -> Z1 l' (Z2 c' r' a')) -> Z3 c r l a -> Z3 c' r' l' a'
wrapZ3 = (Z3 .) . (. fromZ3)

layersFromZ3 :: Z3 c r l a -> Z1 l (Z1 r (Z1 c a))
layersFromZ3 = fmap fromZ2 . fromZ3

instance Functor (Z3 c r l) where
   fmap = wrapZ3 . fmap . fmap

instance (Ord c, Ord r, Ord l, Enum c, Enum r, Enum l) => Applicative (Z3 c r l) where
   fs <*> xs = Z3 $ fmap (<*>) (fromZ3 fs) <*> (fromZ3 xs)
   pure      = Z3 . pure . pure

instance (Ord c, Ord r, Ord l, Enum c, Enum r, Enum l) => ComonadApply (Z3 c r l) where
   (<@>) = (<*>)

instance (Ord c, Ord r, Ord l, Enum c, Enum r, Enum l) => Comonad (Z3 c r l) where
   extract   = view
   duplicate = Z3 . fmap Z2 . widthWise . heightWise . depthWise
      where widthWise  = fmap . fmap $ zipIterate zipL zipR <$> col   <*> id
            heightWise =        fmap $ zipIterate zipU zipD <$> row   <*> id
            depthWise  =               zipIterate zipI zipO <$> level <*> id

instance (Ord c, Ord r, Ord l, Enum c, Enum r, Enum l) => Zipper1 (Z3 c r l a) c where
   zipL = wrapZ3 $ fmap zipL
   zipR = wrapZ3 $ fmap zipR
   col  = index . view . view . layersFromZ3

instance (Ord c, Ord r, Ord l, Enum c, Enum r, Enum l) => Zipper2 (Z3 c r l a) r where
   zipU = wrapZ3 $ fmap zipU
   zipD = wrapZ3 $ fmap zipD
   row  = index . view . layersFromZ3

instance (Ord c, Ord r, Ord l, Enum c, Enum r, Enum l) => Zipper3 (Z3 c r l a) l where
   zipI  = wrapZ3 zipL
   zipO  = wrapZ3 zipR
   level = index . layersFromZ3

instance (Ord c, Ord r, Ord l, Enum c, Enum r, Enum l) => RefOf (Ref c,Ref r,Ref l) (Z3 c r l a) [[[a]]] where
   slice (c,r,l) (c',r',l') = slice l l' . fmap (slice (c,r) (c',r')) . fromZ3
   go (colRef,rowRef,levelRef) = widthWise . heightWise . depthWise
      where
         widthWise  = genericDeref zipL zipR col   colRef
         heightWise = genericDeref zipU zipD row   rowRef
         depthWise  = genericDeref zipI zipO level levelRef

instance (Ord c, Ord r, Ord l, Enum c, Enum r, Enum l) => AnyZipper (Z3 c r l a) (c,r,l) a where
   index = (,,) <$> col <*> row <*> level
   view  = view . view . fromZ3
