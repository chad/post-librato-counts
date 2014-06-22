module Test where
import Librato
import Control.Monad.State


incABunch::StateT Integer IO ()
incABunch = replicateM_ 50 incAndPrintEveryNine
  where incAndPrintEveryNine = incr "genau" 9

main ::IO ((), Integer)
main = runStateT incABunch 0
