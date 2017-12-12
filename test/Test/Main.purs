module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Apply (lift2)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Ef (Ef)
import Control.Monad.Ef.Class (liftEf)
import Data.Traversable (for_)
import Performance.Minibench (benchWith)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Console (CONSOLE)


type TestEff = (console :: CONSOLE)

main :: Eff TestEff Unit
main = liftEf mainF


testLift2 :: Ef TestEff Unit
testLift2 = do
  arr <- mkArr'
  res <- liftEff (pushToArr arr 1) `lift2 plus_` liftEff (pushToArr arr 2)
  res' <- liftEff (pure 1) `lift2 plus_` liftEff (pure 2)
  assert' ([1, 2] == unArr arr) "lift2 1/3"
  assert' (3 == res') "lift2 2/3"
  assert' (3 == res) "lift2 3/3"


testApply :: forall m. MonadEff TestEff m => Int -> m Unit
testApply n' = do
  arr <- liftEff mkArr
  applyLoop (void <<< liftEff <<< pushToArr arr) n'
  liftEff $ assert (naturals n' == unArr arr) $ "apply " <> show n'
  where
  applyLoop :: Monad m => (Int -> m Unit) -> Int -> m Unit
  applyLoop eff max = go (pure unit) 0
    where 
    go acc n | n == max = acc
    go acc n = go (acc <* eff n) (n + 1)



testBindRight :: forall m. MonadEff TestEff m => Int -> m Unit
testBindRight n' = do
  arr <- liftEff mkArr
  bindRightLoop (void <<< liftEff <<< pushToArr arr) n'
  liftEff $ assert (naturals n' == unArr arr) $ "bind right " <> show n'
  where
  bindRightLoop :: Monad m => (Int -> m Unit)  -> Int -> m Unit
  bindRightLoop eff max = go (pure unit) 0
    where 
    go acc n | n == max = acc
    go acc n = go (eff (max - n - 1) >>= const acc) (n + 1)




testBindLeft :: forall m. MonadEff TestEff m => Int -> m Unit
testBindLeft n' = do
  arr <- liftEff mkArr
  bindLeftLoop (void <<< liftEff <<< pushToArr arr) n'
  liftEff $ assert (naturals n' == unArr arr) $ "bind left " <> show n'
  where
  bindLeftLoop :: Monad m => (Int -> m Unit)  -> Int -> m Unit
  bindLeftLoop eff max = go (pure unit) 0
    where 
    go acc n | n == max = acc
    go acc n = go (acc >>= const (eff n)) (n + 1)


testMap :: forall m. MonadEff TestEff m => Int -> m Unit
testMap n = do
  res <- mapLoop n (pure 0)
  liftEff $ assert (res == n) $ "map " <> show n
  where
  mapLoop :: Monad m => Int -> m Int -> m Int
  mapLoop max i = 
    if max == 0 
      then i 
      else mapLoop (max - 1) (map (_ + 1) i)


mainF :: Ef TestEff Unit
mainF = do
  testLift2
  log' "\n"

  test "testBindRight" testBindRight testBindRight
  test "testBindLeft" testMap testMap
  test "testMap" testMap testMap
  test "testApply" testApply testApply
  
  where
  test 
    :: String
    -> (Int -> Eff TestEff Unit)
    -> (Int -> Ef TestEff Unit)
    -> Ef TestEff Unit
  test name eff ef = do
    log' name
    log' "eff"
    liftEff $ eff 100
    log' "Ef"
    liftEf $ ef 100


foreign import data Arr :: Type -> Type


foreign import mkArr :: forall e a. Eff e (Arr a)
mkArr' :: forall e a. Ef e (Arr a)
mkArr' = liftEff mkArr

foreign import pushToArr :: forall e a. Arr a -> a -> Eff e a
pushToArr' :: forall e a. Arr a -> a -> Ef e a
pushToArr' xs x = liftEff $ pushToArr xs x

foreign import assert :: forall e. Boolean -> String -> Eff e Unit
assert' :: forall e. Boolean -> String -> Ef e Unit
assert' b msg = liftEff $ assert b msg

foreign import log :: forall e a. a -> Eff e Unit
log' :: forall e a. a -> Ef e Unit
log' x = liftEff $ log x

timed :: forall m. MonadEff TestEff m => String -> m Unit -> m Unit
timed msg eff = do
  liftEff $ log $ msg <> " ... started"
  liftEff (time msg) *> eff <* liftEff (timeEnd msg)
  liftEff $ log "\n"

foreign import time :: forall e. String -> Eff e Unit
foreign import timeEnd :: forall e. String -> Eff e Unit


foreign import unArr :: forall a. Arr a -> Array a
foreign import naturals :: Int -> Array Int
foreign import plus_ :: Int -> Int -> Int
foreign import stackSize :: Int