-- | This module provides the `bench` function, which prints a short summary
-- | of the running times of a synchronous function to the console. It also
-- | provides the `benchAff` function, which prints a short summary of the
-- | running times of an asynchronous block to the console.
-- |
-- | For benchmarking tasks which require finer accuracy, or graphs as output,
-- | consider using `purescript-benchotron` instead.

module Performance.Minibench
  ( bench
  , benchAff
  , benchAffWith
  , benchWith
  ) where

import Control.Monad.Aff (Aff, makeAff, runAff, nonCanceler)
import Control.Monad.Eff (Eff, runPure)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Uncurried (EffFn1, runEffFn1)
import Data.Array ((..), (:), length)
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Int (toNumber)
import Global (infinity)
import Math (max, min, sqrt)
import Partial.Unsafe (unsafeCrashWith)
import Prelude hiding (min,max)

-- | A wrapper around the Node `process.hrtime()` function.
foreign import hrTime :: forall eff. EffFn1 eff (Array Int) (Array Int)

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
  show (Results results) =
    "mean   = " <> withUnits results.mean <> "\n" <>
    "stddev = " <> withUnits results.stddev <> "\n" <>
    "min    = " <> withUnits results.min' <> "\n" <>
    "max    = " <> withUnits results.max'

-- | Given an array of run timings, calculate the results for the test.
results :: forall eff . Array Number -> Results
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
benchWith
  :: forall eff a
   . Int
  -> (Unit -> a)
  -> Eff (console :: CONSOLE | eff) Unit
benchWith n = runSync n >>> results >>> logShow

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

-- | Run an async Aff n times in sequence, calling the callback with the array
-- | of runtimes when all have completed.
runAsync
  :: forall e a
   . Int
  -> Aff e a
  -> (Array Number -> Eff e Unit)
  -> Eff e Unit
runAsync n aff done = runAsync' n []
  where
    runAsync' 0 runs = done runs
    runAsync' n runs = do
      t1 <- runEffFn1 hrTime [0, 0]
      void $ flip runAff aff \_ -> do
        t2 <- runEffFn1 hrTime t1
        runAsync' (n - 1) ((fromHrTime t2):runs)

-- | Estimate the running time of an Aff and print a summary to the console,
-- | specifying the number of samples to take. More samples will give a better
-- | estimate of both mean and standard deviation, but will increase running
-- | time. Returns an Aff, to make it easy to sequence other asynchronous tests
-- | without interleaving the results.
benchAffWith
  :: forall e a
   . Int
  -> Aff (console :: CONSOLE | e) a
  -> Aff (console :: CONSOLE | e) Unit
benchAffWith n aff = makeAff \success -> do
  runAsync n aff \runs -> do
    logShow $ results runs
    success $ Right unit
  pure nonCanceler

-- | Estimate the running time of an Aff monad and print a summary to the
-- | console, by running the Aff 1000 times. Returns an Aff, to make it easy to
-- | sequence other asynchronous tests without interleaving the results.
-- |
-- | For example:
-- |
-- | ```
-- | > import Data.Array
-- | > import Data.Foldable
-- | > import Performance.Minibench
-- | > void $ launchAff $ benchAff $ readTextFile UTF8 "someFile"
-- |
-- | mean   = 414.00 μs
-- | stddev = 494.82 μs
-- | ```
benchAff
  :: forall e a
   . Aff (console :: CONSOLE | e) a
  -> Aff (console :: CONSOLE | e) Unit
benchAff = benchAffWith defaultNumSamples
