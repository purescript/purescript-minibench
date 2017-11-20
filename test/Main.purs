module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Performance.Minibench (bench, benchAsync, benchAsyncWith, benchWith)

asyncLoop :: forall e. Int -> Eff e Unit -> Eff e Unit
asyncLoop 0 done = done
asyncLoop n done = asyncLoop (n - 1) done

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let loop 0 = 0
      loop n = loop (n - 1)

  log "loop 10"
  benchWith 1000000 \_ -> loop 10
  log "loop 100"
  benchWith 100000 \_ -> loop 100
  log "loop 1000"
  bench \_ -> loop 1000
  log "loop 10000"
  bench \_ -> loop 10000
  log "loop 100000"
  bench \_ -> loop 100000
  log "loop 1000000"
  benchWith 100 \_ -> loop 1000000

  log "bench async loop 10"
  benchAsync (asyncLoop 10) do
    log "bench async loop 10 x10"
    benchAsyncWith 10 (asyncLoop 10) do
      log "bench async loop 1000000"
      benchAsync (asyncLoop 1000000) $ pure unit
