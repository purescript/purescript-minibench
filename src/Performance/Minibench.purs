module Performance.Minibench
  ( bench
  , benchWith
  ) where

import Prelude
import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (modifyRef, newRef, readRef)
import Control.Monad.Eff.Ref.Unsafe (unsafeRunRef)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Data.Int (toNumber)
import Math (sqrt)

foreign import getTime :: forall eff. Eff eff Int

foreign import toExponential :: Number -> String

measure :: forall eff a. Eff eff a -> Eff eff Int
measure e = do
  t1 <- getTime
  _ <- e
  t2 <- getTime
  pure (t2 - t1)

withUnits :: Number -> String
withUnits t
  | t < 1.0 = toExponential (t * 1000.0) <> " ms"
  | otherwise = toExponential t <> " s"

benchWith
  :: forall eff a
   . Int
  -> Eff (console :: CONSOLE | eff) a
  -> Eff (console :: CONSOLE | eff) Unit
benchWith n f = unsafeRunRef do
  sumRef <- newRef 0.0
  sum2Ref <- newRef 0.0
  forE 0 n \_ -> do
    t <- measure (unsafeCoerceEff f)
    let seconds = toNumber t / 1000.0
        square  = seconds * seconds
    modifyRef sumRef (_ + seconds)
    modifyRef sum2Ref (_ + square)
  sum <- readRef sumRef
  sum2 <- readRef sum2Ref
  let n'     = toNumber n
      mean   = sum / n'
      stdDev = sqrt ((sum2 - n' * mean * mean) / (n' - 1.0))
  log ("mean   = " <> withUnits mean)
  log ("stddev = " <> withUnits stdDev)

bench
  :: forall eff a
   . Eff (console :: CONSOLE | eff) a
  -> Eff (console :: CONSOLE | eff) Unit
bench = benchWith 1000
