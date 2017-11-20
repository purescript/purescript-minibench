-- | This module provides the `bench` function, which prints a short summary
-- | of the running times of a synchronous function to the console. It also
-- | provides the `benchAsync` function, which prints a short summary of the
-- | running times of an asynchronous function to the console.
-- |
-- | For benchmarking tasks which require finer accuracy, or graphs as output,
-- | consider using `purescript-benchotron` instead.

module Performance.Minibench
  ( bench
  , benchAsync
  , benchAsyncWith
  , benchWith
  ) where

import Control.Monad.Eff (Eff, runPure)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Uncurried (EffFn1, runEffFn1)
import Data.Array ((..), (:), length)
import Data.Foldable (foldr)
import Data.Int (toNumber)
import Global (infinity)
import Math (max, min, sqrt)
import Partial.Unsafe (unsafeCrashWith)
import Prelude hiding (min,max)

-- | A wrapper around the Node `process.hrtime()` function.
foreign import hrTime :: forall eff. EffFn1 eff (Array Int) (Array Int)

-- | Force garbage collection.
-- | Requires node to be run with the --force-gc flag.
foreign import gc :: forall eff. Eff eff Unit

foreign import toFixed :: Number -> String

fromHrTime :: Array Int -> Number
fromHrTime [s, ns] = toNumber s * 1.0e9 + toNumber ns
fromHrTime _ = unsafeCrashWith "fromHrTime: unexpected result from process.hrtime()"

withUnits :: Number -> String
withUnits t
  | t < 1.0e3 = toFixed t <> " ns"
  | t < 1.0e6 = toFixed (t / 1.0e3) <> " μs"
  | t < 1.0e9 = toFixed (t / 1.0e6) <> " ms"
  | otherwise = toFixed (t / 1.0e9) <> " s"

-- | When running `bench` or `benchAff`, this is the number of samples that will
-- | be used.
defaultNumSamples :: Int
defaultNumSamples = 1000

-- | The `Results` type is a `Record` newtype that contains information about
-- | the timings for the test run.
newtype Results = Results
  { mean :: Number
  , stddev :: Number
  , min' :: Number
  , max' :: Number
  }

instance showResults :: Show Results where
  show (Results results') =
    "mean   = " <> withUnits results'.mean <> "\n" <>
    "stddev = " <> withUnits results'.stddev <> "\n" <>
    "min    = " <> withUnits results'.min' <> "\n" <>
    "max    = " <> withUnits results'.max'

-- | Given an array of run timings, calculate the results for the test.
results :: Array Number -> Results
results runs = Results {mean, stddev, min', max'}
  where
    sum = foldr (+) 0.0 runs
    sum2 = foldr (\cur -> (+) (cur * cur)) 0.0 runs
    n = toNumber $ length runs
    mean = sum / n
    stddev = sqrt ((sum2 - n * mean * mean) / (n - 1.0))
    min' = foldr min infinity runs
    max' = foldr max 0.0 runs

-- | Run a synchronous function n times, returning the array of runtimes.
runSync :: forall a. Int -> (Unit -> a) -> Array Number
runSync n f = flip map (0..n) \_ -> fromHrTime $ runPure do
  t1 <- runEffFn1 hrTime [0, 0]
  const (runEffFn1 hrTime t1) (f unit)

-- | Estimate the running time of a function and print a summary to the console,
-- | specifying the number of samples to take. More samples will give a better
-- | estimate of both mean and standard deviation, but will increase running time.
-- |
-- | To increase benchmark accuracy by forcing garbage collection before the
-- | benchmark is run, node should be invoked with the '--expose-gc' flag.
benchWith
  :: forall eff a
   . Int
  -> (Unit -> a)
  -> Eff (console :: CONSOLE | eff) Unit
benchWith n f = gc >>= \_ -> logShow $ results $ runSync n f

-- | Estimate the running time of a function and print a summary to the console,
-- | by running the function 1000 times.
-- |
-- | For example:
-- |
-- | ```
-- | > import Data.Array
-- | > import Data.Foldable
-- | > import Performance.Minibench
-- | > bench \_ -> sum (1 .. 10000)
-- |
-- | mean   = 414.00 μs
-- | stddev = 494.82 μs
-- | ```
bench
  :: forall eff a
   . (Unit -> a)
  -> Eff (console :: CONSOLE | eff) Unit
bench = benchWith defaultNumSamples

-- | Run an asynchronous function n times sequentially, calling the given
-- | callback with the array of runtimes after all runs are completed.
runAsync
  :: forall e
   . Int
  -> (Eff (console :: CONSOLE | e) Unit -> Eff (console :: CONSOLE | e) Unit)
  -> (Array Number -> Eff (console :: CONSOLE | e) Unit)
  -> Eff (console :: CONSOLE | e) Unit
runAsync n f done = runAsync' n []
  where
    runAsync' 0 runs = done runs
    runAsync' m runs = do
      t1 <- runEffFn1 hrTime [0, 0]
      f do
        t2 <- runEffFn1 hrTime t1
        void $ runAsync' (m - 1) ((fromHrTime t2):runs)

-- | Estimate the running time of an asynchronous function and print a summary
-- | to the console, specifying the number of samples to take. More samples will
-- | give a better estimate of both mean and standard deviation, but will
-- | increase running time.
benchAsyncWith
  :: forall e
   . Int
  -> (Eff (console :: CONSOLE | e) Unit -> Eff (console :: CONSOLE | e) Unit)
  -> Eff (console :: CONSOLE | e) Unit
  -> Eff (console :: CONSOLE | e) Unit
benchAsyncWith n f done =
  runAsync n f \runs -> logShow (results runs) >>= \_ -> done

-- | Estimate the running time of an asynchronous function and print a summary
-- | to the console, by running the function 1000 times.
-- |
-- | For example:
-- |
-- | ```
-- | > import Data.Array
-- | > import Data.Foldable
-- | > import Performance.Minibench
-- | > benchAsync \done -> sum (1 .. 10000) >>= \_ -> done
-- |
-- | mean   = 414.00 μs
-- | stddev = 494.82 μs
-- | ```
benchAsync
  :: forall e
   . (Eff (console :: CONSOLE | e) Unit -> Eff (console :: CONSOLE | e) Unit)
  -> Eff (console :: CONSOLE | e) Unit
  -> Eff (console :: CONSOLE | e) Unit
benchAsync = benchAsyncWith defaultNumSamples
