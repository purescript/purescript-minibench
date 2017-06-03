module Test.Main where

import Prelude
import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Performance.Minibench (bench)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "forE 1000"
  bench $ forE 0 1000 (\_ -> pure unit)
  log "forE 10000"
  bench $ forE 0 10000 (\_ -> pure unit)
  log "forE 100000"
  bench $ forE 0 100000 (\_ -> pure unit)
