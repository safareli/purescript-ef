module Bench.Main where

import Prelude

import Control.Monad.Ef (Ef)
import Control.Monad.Ef.Class (liftEf)
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Traversable (for_, intercalate)
import Performance.Minibench (BenchResult, benchWith', withUnits)


type BenchEff = (console :: CONSOLE)

testApply :: forall m. MonadEff BenchEff m => Int -> m Unit
testApply n' = do
  arr <- liftEff mkArr
  applyLoop (void <<< liftEff <<< pushToArr arr) n'
  where
  applyLoop :: Monad m => (Int -> m Unit) -> Int -> m Unit
  applyLoop eff max = go (pure unit) 0
    where 
    go acc n | n == max = acc
    go acc n = go (acc <* eff n) (n + 1)


testBindRight :: forall m. MonadEff BenchEff m => Int -> m Unit
testBindRight n' = do
  arr <- liftEff mkArr
  bindRightLoop (void <<< liftEff <<< pushToArr arr) n'
  where
  bindRightLoop :: Monad m => (Int -> m Unit)  -> Int -> m Unit
  bindRightLoop eff max = go (pure unit) 0
    where 
    go acc n | n == max = acc
    go acc n = go (eff (max - n - 1) >>= const acc) (n + 1)


testBindLeft :: forall m. MonadEff BenchEff m => Int -> m Unit
testBindLeft n' = do
  arr <- liftEff mkArr
  bindLeftLoop (void <<< liftEff <<< pushToArr arr) n'
  where
  bindLeftLoop :: Monad m => (Int -> m Unit)  -> Int -> m Unit
  bindLeftLoop eff max = go (pure unit) 0
    where 
    go acc n | n == max = acc
    go acc n = go (acc >>= const (eff n)) (n + 1)


testMap :: forall m. MonadEff BenchEff m => Int -> m Unit
testMap n = do
  res <- mapLoop n (pure 0)
  pure unit
  where
  mapLoop :: Monad m => Int -> m Int -> m Int
  mapLoop max i = 
    if max == 0 
      then i 
      else mapLoop (max - 1) (map (_ + 1) i)


main :: Eff BenchEff Unit
main = do
  log header
  bench3 ">>=R" testBindRight testBindRight testBindRight [100, 500, 1000, 2000, 4000, 8000, 10000]
  bench3 ">>=L" testBindLeft testBindLeft testBindLeft [100, 500, 1000, 2000, 4000, 8000]
  bench3 "map" testMap testMap testMap [100, 500, 1000, 2000, 4000, 5000]
  bench3 "apply" testApply testApply testApply [100, 500, 1000, 2000, 4000, 5000]

extended :: Eff BenchEff Unit
extended = do
  log header
  bench2 ">>=R" testBindRight testBindRight [20000, 50000, 100000, 1000000]
  bench2 ">>=L" testBindLeft testBindLeft [20000, 50000, 100000, 1000000]
  bench2 "map" testMap testMap [10000, 20000, 50000, 100000, 1000000, 10000000]
  bench2 "apply" testApply testApply [10000, 20000, 50000, 100000, 1000000]

header :: String
header = 
  "| bench | type | n | mean | stddev | min | max |\n" <>
  "| ----- | ---- | - | ---- | ------ | --- | --- |"

bench3
  :: String
  -> (Int -> Eff BenchEff Unit)
  -> (Int -> Ef BenchEff Unit)
  -> (Int -> Aff BenchEff Unit)
  -> Array Int
  -> Eff BenchEff Unit
bench3 name buildEff buildEf buildAff vals = for_ vals \val -> do 
  logBench [name <> " build", "Eff", show val] $ benchWith' 1000 \_ -> buildEff val
  logBench [name <> " build", "Aff", show val] $ benchWith' 1000 \_ -> buildAff val
  logBench' [name <> " build", "Ef", show val] $ benchWith' 1000 \_ -> buildEf val
  let eff = liftEff $ buildEff val
  logBench [name <> " run", "Eff", show val] $ benchWith' 1000 \_ -> unsafePerformEff eff
  let aff = buildAff val
  logBench [name <> " run", "Aff", show val] $ benchWith' 1000 \_ -> unsafePerformEff $ launchAff_ aff
  let ef = liftEf $ buildEf val
  logBench' [name <> " run", "Ef", show val] $ benchWith' 1000 \_ -> unsafePerformEff ef

bench2
  :: String
  -> (Int -> Ef BenchEff Unit)
  -> (Int -> Aff BenchEff Unit)
  -> Array Int
  -> Eff BenchEff Unit
bench2 name buildEf buildAff vals = for_ vals \val -> do 
  logBench [name <> " build", "Aff", show val] $ benchWith' 4 \_ -> buildAff val
  logBench' [name <> " build", "Ef", show val] $ benchWith' 4 \_ -> buildEf val
  let aff = buildAff val
  logBench [name <> " run", "Aff", show val] $ benchWith' 4 \_ -> unsafePerformEff $ launchAff_ aff
  let ef = liftEf $ buildEf val
  logBench' [name <> " run", "Ef", show val] $ benchWith' 4 \_ -> unsafePerformEff ef

logBench'' :: (String -> String) -> Array String -> Eff BenchEff BenchResult -> Eff BenchEff Unit
logBench'' f msg benchEff = do
  res <- benchEff
  let 
    logStr = intercalate " | " 
      $ append msg 
      $ map (f <<< withUnits) [res.mean, res.stdDev, res.min, res.max]
  log $  "| "  <> logStr <>  " |"

logBench :: Array String -> Eff BenchEff BenchResult -> Eff BenchEff Unit
logBench = logBench'' id

logBench' :: Array String -> Eff BenchEff BenchResult -> Eff BenchEff Unit
logBench' = logBench'' \s -> "**" <> s <> "**"

foreign import data Arr :: Type -> Type

foreign import mkArr :: forall e a. Eff e (Arr a)
foreign import pushToArr :: forall e a. Arr a -> a -> Eff e a
foreign import log :: forall e a. a -> Eff e Unit

