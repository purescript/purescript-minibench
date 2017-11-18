module Test.Main where

import Prelude
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Aff.Console as AffConsole
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Aff (readTextFile)
import Performance.Minibench (bench, benchAff, benchWith, benchAffWith)

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

  void $ launchAff do
    AffConsole.log "bench aff"
    benchAff $ readTextFile UTF8 "./test/mockFile"
    AffConsole.log "bench aff triple read"
    benchAff do
      _ <- readTextFile UTF8 "./test/mockFile"
      _ <- readTextFile UTF8 "./test/mockFile"
      readTextFile UTF8 "./test/mockFile"
    AffConsole.log "bench aff x10"
    benchAffWith 10 $ readTextFile UTF8 "./test/mockFile"
