module Cartesian where

import Control.Applicative
import Data.Functor.Compose

import Tape

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
