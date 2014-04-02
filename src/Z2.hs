{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Z2
   ( module Generic , Z2(..) , wrapZ2
   ) where

import Generic
import Z1

newtype Z2 c r a = Z2 { fromZ2 :: Z1 r (Z1 c a) }

wrapZ2 :: (Z1 r (Z1 c a) -> Z1 r' (Z1 c' a')) -> Z2 c r a -> Z2 c' r' a'
wrapZ2 = (Z2 .) . (. fromZ2)

instance Functor (Z2 c r) where
   fmap = wrapZ2 . fmap . fmap

instance (Ord c, Ord r, Enum c, Enum r) => Applicative (Z2 c r) where
   fs <*> xs = Z2 $ fmap (<*>) (fromZ2 fs) <*> (fromZ2 xs)
   pure      = Z2 . pure . pure

instance (Ord c, Ord r, Enum c, Enum r) => ComonadApply (Z2 c r) where
   (<@>) = (<*>)

instance (Ord c, Ord r, Enum c, Enum r) => Comonad (Z2 c r) where
   extract   = view
   duplicate = Z2 . widthWise . heightWise
      where widthWise  = fmap $ zipIterate zipL zipR <$> col <*> id
            heightWise =        zipIterate zipU zipD <$> row <*> id

instance (Ord c, Ord r, Enum c, Enum r) => Zipper1 (Z2 c r a) c where
   zipL = wrapZ2 $ fmap zipL
   zipR = wrapZ2 $ fmap zipR
   col  = index . view . fromZ2

instance (Ord c, Ord r, Enum c, Enum r) => Zipper2 (Z2 c r a) r where
   zipU = wrapZ2 zipL
   zipD = wrapZ2 zipR
   row  = index . fromZ2

instance (Ord c, Enum c, Ord r, Enum r) => RefOf (Ref c,Ref r) (Z2 c r a) [[a]] where
   slice (c,r) (c',r') = slice r r' . fmap (slice c c') . fromZ2
   insert list z = Z2 $ insert <$> (insert list (pure [])) <*> fromZ2 z
   go (colRef,rowRef) = horizontal . vertical
      where
         horizontal = genericDeref zipL zipR col colRef
         vertical   = genericDeref zipU zipD row rowRef

instance (Ord c, Enum c, Ord r, Enum r) => AnyZipper (Z2 c r a) (c,r) a where
   index = (,) <$> col <*> row
   view  = view . view . fromZ2
   write = wrapZ2 . modify . write
   reindex (c,r) = wrapZ2 (fmap (reindex c) . reindex r)

modifyCell :: (Ord c, Enum c, Ord r, Enum r) => (a -> a) -> Z2 c r a -> Z2 c r a
modifyCell f = write <$> f . view <*> id

modifyRow :: (Z1 c a -> Z1 c a) -> Z2 c r a -> Z2 c r a
modifyRow = wrapZ2 . modify

modifyCol :: (Enum r, Ord r) => (Z1 r a -> Z1 r a) -> Z2 c r a -> Z2 c r a
modifyCol f = writeCol <$> f . fmap view . fromZ2 <*> id

writeRow :: Z1 c a -> Z2 c r a -> Z2 c r a
writeRow = wrapZ2 . write

writeCol :: (Ord r, Enum r) => Z1 r a -> Z2 c r a -> Z2 c r a
writeCol c plane = Z2 $ write <$> c <*> fromZ2 plane

insertRowU, insertRowD :: Z1 c a -> Z2 c r a -> Z2 c r a
insertRowU = wrapZ2 . insertL
insertRowD = wrapZ2 . insertR

deleteRowD, deleteRowU :: Z2 c r a -> Z2 c r a
deleteRowD = wrapZ2 deleteR
deleteRowU = wrapZ2 deleteL

insertColL, insertColR :: (Ord r, Enum r) => Z1 r a -> Z2 c r a -> Z2 c r a
insertColL c plane = Z2 $ insertL <$> c <*> fromZ2 plane
insertColR c plane = Z2 $ insertR <$> c <*> fromZ2 plane

deleteColL, deleteColR :: (Ord r, Enum r) => Z2 c r a -> Z2 c r a
deleteColL = wrapZ2 $ fmap deleteL
deleteColR = wrapZ2 $ fmap deleteR

insertCellD, insertCellU :: (Ord r, Enum r) => a -> Z2 c r a -> Z2 c r a
insertCellD = modifyCol . insertR
insertCellU = modifyCol . insertL

insertCellR, insertCellL :: (Ord c, Enum c) => a -> Z2 c r a -> Z2 c r a
insertCellR = modifyRow . insertR
insertCellL = modifyRow . insertL

deleteCellD, deleteCellU :: (Ord r, Enum r) => Z2 c r a -> Z2 c r a
deleteCellD = modifyCol deleteR
deleteCellU = modifyCol deleteL

deleteCellR, deleteCellL :: (Ord c, Enum c) => Z2 c r a -> Z2 c r a
deleteCellR = modifyRow deleteR
deleteCellL = modifyRow deleteL

--TODO...
--insertCellsR, insertCellsL, insertCellsU, insertCellsD
--insertColsR, insertColsL, insertRowsU, insertRowsD
