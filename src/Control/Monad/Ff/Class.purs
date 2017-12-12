module Control.Monad.Ef.Class where

import Control.Category (id)
import Control.Monad (class Monad)
import Control.Monad.Ef (Ef, toEff)
import Control.Monad.Eff (Eff)

class Monad m <= MonadEf eff m | m -> eff where
  liftEf :: forall a. Ef eff a -> m a

instance monadEfEf :: MonadEf eff (Ef eff) where
  liftEf = id

instance monadEfEff :: MonadEf eff (Eff eff) where
  liftEf = toEff
