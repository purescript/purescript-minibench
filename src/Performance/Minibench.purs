-- | This module provides the `bench` function, which prints a short summary
-- | of the running times of a synchronous function to the console.
-- |
-- | For benchmarking tasks which require finer accuracy, or graphs as output,
-- | consider using `purescript-benchotron` instead.

module Performance.Minibench
  ( bench
  , benchWith
  ) where

import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Uncurried (EffFn1, runEffFn1)
import Control.Monad.ST (modifySTRef, newSTRef, readSTRef, runST)
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
benchWith n f = runST do
  sumRef <- newSTRef 0.0
  sum2Ref <- newSTRef 0.0
  minRef <- newSTRef infinity
  maxRef <- newSTRef 0.0
  gc
  forE 0 n \_ -> do
    t1 <- runEffFn1 hrTime [0, 0]
    t2 <- const (runEffFn1 hrTime t1) (f unit)
    let ns     = fromHrTime t2
        square = ns * ns
    _ <- modifySTRef sumRef (_ + ns)
    _ <- modifySTRef sum2Ref (_ + square)
    _ <- modifySTRef minRef (_ `min` ns)
    _ <- modifySTRef maxRef (_ `max` ns)
    pure unit
  sum <- readSTRef sumRef
  sum2 <- readSTRef sum2Ref
  min' <- readSTRef minRef
  max' <- readSTRef maxRef
  let n'     = toNumber n
      mean   = sum / n'
      stdDev = sqrt ((sum2 - n' * mean * mean) / (n' - 1.0))
  log ("mean   = " <> withUnits mean)
  log ("stddev = " <> withUnits stdDev)
  log ("min    = " <> withUnits min')
  log ("max    = " <> withUnits max')

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
bench = benchWith 1000
