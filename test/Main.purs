module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Async (readTextFile)
import Performance.Minibench (bench, benchAsync, benchAsyncWith, benchWith)

asyncReadFile :: forall e . Eff (fs :: FS | e) Unit -> Eff (fs :: FS | e) Unit
asyncReadFile done = readTextFile UTF8 "./test/mockFile" \_ -> done

main :: forall e. Eff (console :: CONSOLE, fs :: FS | e) Unit
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

  log "bench aff"
  benchAsync asyncReadFile do
    log "bench aff x10"
    benchAsyncWith 10 asyncReadFile do
      log "bench aff triple read"
      benchAsync (asyncReadFile >>> asyncReadFile >>> asyncReadFile) $ pure unit
