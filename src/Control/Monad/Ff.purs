module Control.Monad.Ef (Ef) where

import Control.Applicative (class Applicative)
-- import Control.Applicative (class Applicative, liftA1)
import Control.Apply (class Apply)
import Control.Bind (class Bind)
import Control.Monad (class Monad)
-- import Control.Monad (class Monad, ap)
import Control.Monad.Eff (kind Effect)
import Data.Functor (class Functor)
import Control.Monad.Eff.Class (class MonadEff)
import Unsafe.Coerce (unsafeCoerce)


foreign import data Ef :: # Effect -> Type -> Type

instance functorEf :: Functor (Ef e) where
  map = mapE
  -- map = liftA1

instance applyEf :: Apply (Ef e) where
  apply = applyE
  -- apply = ap

instance applicativeEf :: Applicative (Ef e) where
  pure = pureE

instance bindEf :: Bind (Ef e) where
  bind = bindE

instance monadEf :: Monad (Ef e)

instance monadEEFff :: MonadEff eff (Ef eff) where
  liftEff = unsafeCoerce

foreign import mapE :: forall e a b. (a -> b) -> Ef e a -> Ef e b
foreign import applyE :: forall e a b. Ef e (a -> b) -> Ef e a-> Ef e b
foreign import pureE :: forall e a. a -> Ef e a
foreign import bindE :: forall e a b. Ef e a -> (a -> Ef e b) -> Ef e b