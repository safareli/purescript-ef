module Bench.Main where

import Prelude

import Control.Monad.Ef (Ef)
import Control.Monad.Ef.Class (liftEf)
import Control.Monad.Eff (Eff)
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
  bench "bind assocR" testBindRight testBindRight [100, 500, 1000, 2000, 4000, 8000, 10000]
  bench "bind assocL" testMap testMap [100, 500, 1000, 2000, 4000, 8000]
  bench "map" testMap testMap [100, 500, 1000, 2000, 4000, 5000]
  bench "apply" testApply testApply [100, 500, 1000, 2000, 4000, 5000]

extended :: Eff BenchEff Unit
extended = do
  log header
  timed ["bind assocR", "Ef", "100000"] $ testBindRight 100000 
  timed ["bind assocR", "Ef", "1000000"] $ testBindRight 1000000 -- ~ 1 sec
--timed ["bind assocR", "Ef", "10000000"] $ testBindRight 10000000 -- ~ 10 sec
--timed ["bind assocR", "Ef", "100000000"] $ testBindRight 100000000  -- JavaScript heap out of memory  
  timed ["bind assocL", "Ef", "20000"] $ testBindLeft 20000
  timed ["bind assocL", "Ef", "40000"] $ testBindLeft 40000
  timed ["bind assocL", "Ef", "80000"] $ testBindLeft 80000
  timed ["map", "Ef", "10000"] $ testMap 10000
  timed ["map", "Ef", "20000"] $ testMap 20000
  timed ["map", "Ef", "40000"] $ testMap 40000
  timed ["map", "Ef", "80000"] $ testMap 80000
  timed ["apply", "Ef", "10000"] $ testApply 10000
  timed ["apply", "Ef", "20000"] $ testApply 20000
  timed ["apply", "Ef", "40000"] $ testApply 40000

header :: String
header = 
  "| bench | type | n | mean | stddev | min | max |\n" <>
  "| ----- | ---- | - | ---- | ------ | --- | --- |"

bench
  :: String
  -> (Int -> Eff BenchEff Unit)
  -> (Int -> Ef BenchEff Unit)
  -> Array Int
  -> Eff BenchEff Unit
bench name buildEff buildEf vals = for_ vals \val -> do 
  logBench [name <> " build", "Eff", show val] $ benchWith' 2000 \_ -> buildEff val
  logBench' [name <> " build", "Ef", show val] $ benchWith' 2000 \_ -> buildEf val
  let eff = liftEff $ buildEff val
  logBench [name <> " run", "Eff", show val] $ benchWith' 2000 \_ -> unsafePerformEff eff
  let ef = liftEf $ buildEf val
  logBench' [name <> " run", "Ef", show val] $ benchWith' 2000 \_ -> unsafePerformEff ef


timed :: Array String -> Ef BenchEff Unit -> Eff BenchEff Unit
timed msg eff = 
  logBench' msg $ benchWith' 5 \_ -> unsafePerformEff $ liftEf eff

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

