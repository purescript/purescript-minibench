module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Performance.Minibench (bench, benchWith)

main :: Effect Unit
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
