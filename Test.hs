module Test where
import Librato
import Control.Monad.State


incABunch::StateT Integer IO Integer
incABunch = do
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  incAndPrintEveryNine
  where incAndPrintEveryNine = incr "genau"  9
  
main = do runStateT incABunch 0
