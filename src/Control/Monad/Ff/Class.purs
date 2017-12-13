module Control.Monad.Ef.Class
  ( class MonadEf
  , liftEf
  ) where

import Control.Category (id)
import Control.Monad (class Monad)
import Control.Monad.Ef (Ef)
import Control.Monad.Eff (Eff)
import Unsafe.Coerce (unsafeCoerce)

class Monad m <= MonadEf eff m | m -> eff where
  liftEf :: forall a. Ef eff a -> m a

instance monadEfEf :: MonadEf eff (Ef eff) where
  liftEf = id

instance monadEfEff :: MonadEf eff (Eff eff) where
  liftEf = unsafeCoerce