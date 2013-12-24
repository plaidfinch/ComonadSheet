{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Z4
   ( module Generic , Z4(..) , wrapZ4
   ) where

import Generic
import Z1
import Z2
import Z3

newtype Z4 c r l s a = Z4 { fromZ4 :: Z1 s (Z3 c r l a) }

wrapZ4 :: (Z1 s (Z3 c r l a) -> Z1 s' (Z3 c' r' l' a')) -> Z4 c r l s a -> Z4 c' r' l' s' a'
wrapZ4 = (Z4 .) . (. fromZ4)

layersFromZ4 :: Z4 c r l s a -> Z1 s (Z1 l (Z1 r (Z1 c a)))
layersFromZ4 = (fmap . fmap) fromZ2 . fmap fromZ3 . fromZ4

layersToZ4 :: Z1 s (Z1 l (Z1 r (Z1 c a))) -> Z4 c r l s a
layersToZ4 = Z4 . fmap Z3 . (fmap . fmap) Z2

instance Functor (Z4 c r l s) where
   fmap = wrapZ4 . fmap . fmap

instance (Ord c, Ord r, Ord l, Ord s, Enum c, Enum r, Enum l, Enum s) => Applicative (Z4 c r l s) where
   fs <*> xs = Z4 $ fmap (<*>) (fromZ4 fs) <*> (fromZ4 xs)
   pure      = Z4 . pure . pure

instance (Ord c, Ord r, Ord l, Ord s, Enum c, Enum r, Enum l, Enum s) => ComonadApply (Z4 c r l s) where
   (<@>) = (<*>)

instance (Ord c, Ord r, Ord l, Ord s, Enum c, Enum r, Enum l, Enum s) => Comonad (Z4 c r l s) where
   extract   = view
   duplicate = layersToZ4 . widthWise . heightWise . depthWise . splissWise
      where widthWise  = fmap . fmap . fmap $ zipIterate zipL zipR <$> col   <*> id
            heightWise =        fmap . fmap $ zipIterate zipU zipD <$> row   <*> id
            depthWise  =               fmap $ zipIterate zipI zipO <$> level <*> id
            splissWise =                      zipIterate zipA zipK <$> space <*> id

instance (Ord c, Ord r, Ord l, Ord s, Enum c, Enum r, Enum l, Enum s) => Zipper1 (Z4 c r l s a) c where
   zipL = wrapZ4 $ fmap zipL
   zipR = wrapZ4 $ fmap zipR
   col  = index . view . view . view . layersFromZ4

instance (Ord c, Ord r, Ord l, Ord s, Enum c, Enum r, Enum l, Enum s) => Zipper2 (Z4 c r l s a) r where
   zipU = wrapZ4 $ fmap zipU
   zipD = wrapZ4 $ fmap zipD
   row  = index . view . view . layersFromZ4

instance (Ord c, Ord r, Ord l, Ord s, Enum c, Enum r, Enum l, Enum s) => Zipper3 (Z4 c r l s a) l where
   zipI  = wrapZ4 $ fmap zipI
   zipO  = wrapZ4 $ fmap zipO
   level = index . view . layersFromZ4

instance (Ord c, Ord r, Ord l, Ord s, Enum c, Enum r, Enum l, Enum s) => Zipper4 (Z4 c r l s a) s where
   zipA  = wrapZ4 zipL
   zipK  = wrapZ4 zipR
   space = index . layersFromZ4

instance (Ord c, Ord r, Ord l, Ord s, Enum c, Enum r, Enum l, Enum s) => RefOf (Ref c,Ref r,Ref l,Ref s) (Z4 c r l s a) [[[[a]]]] where
   slice (c,r,l,s) (c',r',l',s') = slice s s' . fmap (slice (c,r,l) (c',r',l')) . fromZ4
   insert list zipper = Z4 $ insert <$> (insert list (pure [])) <*> fromZ4 zipper
   go (colRef,rowRef,levelRef,spaceRef) = widthWise . heightWise . depthWise . splissWise
      where
         widthWise  = genericDeref zipL zipR col   colRef
         heightWise = genericDeref zipU zipD row   rowRef
         depthWise  = genericDeref zipI zipO level levelRef
         splissWise = genericDeref zipA zipK space spaceRef

instance (Ord c, Ord r, Ord l, Ord s, Enum c, Enum r, Enum l, Enum s) => AnyZipper (Z4 c r l s a) (c,r,l,s) a where
   index = (,,,) <$> col <*> row <*> level <*> space
   view  = view . view . fromZ4
   write = wrapZ4 . modify . write
   reindex (c,r,l,s) = wrapZ4 (fmap (reindex (c,r,l)) . reindex s)
