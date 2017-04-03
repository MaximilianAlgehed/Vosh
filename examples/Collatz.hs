{-# LANGUAGE NoImplicitPrelude #-}
module Collatz where

import qualified Prelude as Meta
import           Vosh

sz :: Int
sz = 16

a, an :: Vec
a  = v <$> avar
an = v <$> anvar

avar, anvar :: [Int]
avar  = [ i Meta.* 2          | i <- [ 0 .. (sz Meta.- 1) ] ]
anvar = [ i Meta.* 2 Meta.+ 1 | i <- [ 0 .. (sz Meta.- 1) ] ]

collatz :: Vec -> Vec
collatz v =
  ife (isOdd v)
      (v + v + v + int2vec sz 1)
      (v >>> 1)

transitionRelation :: BDD
transitionRelation = collatz a == an

var_order :: [Int]
var_order = interleave (reverse avar) (reverse anvar)

a2n_sub :: [(Int, BDD)]
a2n_sub = zip avar an

a2n :: BDD -> BDD
a2n f = substitute a2n_sub f

rangeLimit :: BDD 
rangeLimit =  int2vec sz 1 <= a
           && a <= (int2vec sz (2 Meta.^ (sz Meta.- 4)))

ok0 :: BDD
ok0 = a == int2vec sz 1

reach :: Int -> BDD -> BDD
reach n ok =
  let okn = a2n ok
      new = ok || rangeLimit && exists (foldl (&&) true an) (transitionRelation && okn)
  in if n Meta.== 0
     then new
     else reach (n Meta.- 1) new
