module Main where

import Cudd.Cudd

main = do
  let manager = cuddInit
      v1      = ithVar cuddInit 0
      v2      = ithVar cuddInit 1
      conj    = bAnd cuddInit v1 v2
      implies = lEq cuddInit conj v1
  print implies
