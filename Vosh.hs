module Vosh where

import Cudd.Cudd

import System.IO
import System.Process

import qualified Prelude   as P
import qualified Data.Bool as Bool
import           Prelude   (($), (.))
import           Data.Bool (Bool, otherwise)
import           Data.Int
import           Data.List


newtype BDD = BDD { runBDD :: DDManager -> DDNode }

class Boolean a where
  true, false :: a
  false = not true
  (&&) :: a -> a -> a
  not  :: a -> a
  (||) :: a -> a -> a
  a || b = not (not a && not b)
  xor  :: a -> a -> a
  a `xor` b = (not a && b) || (a && not b)
  xnor :: a -> a -> a
  a `xnor` b = not (a `xor` b)
  (==>) :: a -> a -> a
  a ==> b = (not a) || b

infixr 3 &&
infixr 2 ||

nullary :: (DDManager -> DDNode) -> BDD
nullary = BDD

unary :: (DDManager -> DDNode -> DDNode) -> BDD -> BDD
unary foo bdd = BDD $ \m -> foo m (runBDD bdd m)

binary :: (DDManager -> DDNode -> DDNode -> DDNode) -> BDD -> BDD -> BDD
binary op f g = BDD $ \m -> op m (runBDD f m) (runBDD g m)

instance Boolean BDD where
  true  = nullary readOne 
  false = nullary readLogicZero
  (&&)  = binary  bAnd
  (||)  = binary  bOr
  not   = unary   bNot
  xor   = binary  bXor
  xnor  = binary  bXnor

instance Boolean Bool where
  true  = Bool.True
  false = Bool.False
  (&&)  = (Bool.&&)
  (||)  = (Bool.||)
  not   = Bool.not

-- | Create a BDD variable
v :: Int -> BDD
v i = BDD $ \m -> ithVar m i

-- | Evaluate a BDD expression
evaluate :: DDManager -> BDD -> [Bool] -> Bool
evaluate m bdd ass = eval m (runBDD bdd m) [ if a then 1 else 0 | a <- ass]

-- | Display a BDD
-- TODO: Use `dumpDot'` to avoid an intermidiary file
display :: DDManager -> BDD -> IO ()
display m bdd = do
  dumpDot m (runBDD bdd m) "/tmp/graph.dot"
  callCommand "dot -Txlib /tmp/graph.dot"

{- Some functions for numbers -}
-- TODO:
--  * Change the ordering to have `head` be the zero:th bit
--  * Index the type by their size
type Vec = [BDD]

-- | `int2vec size number = number_as_bitvec`
int2vec :: Int -> Int -> Vec
int2vec sz i = reverse (take sz (ivec i))
  where
    ivec i = ivec' i ++ repeat false

    ivec' 0 = []
    ivec' n
        | P.even n = false : ivec' (n `P.div` 2)
        | P.odd  n = true  : ivec' (n `P.div` 2)

-- | ha a b = (cout, sum)
ha :: BDD -> BDD -> (BDD, BDD)
ha a b = (a && b, a `xor` b)

-- | fa cin a b = (cout, sum)
fa :: BDD -> BDD -> BDD -> (BDD, BDD)
fa cin a b = let (cout1, sum1) = ha a b
                 (cout2, sum2) = ha sum1 cin
             in  (cout1 || cout2, sum2)

-- | Two's compliment
negate :: Vec -> Vec
negate xs = let sz = length xs in
  map not xs + int2vec sz 1

-- | Ripple Carry Adder
(+) :: Vec -> Vec -> Vec
as + bs = reverse (plus false (reverse as) (reverse bs))
  where
    plus cin [] []         = []
    plus cin (a:as) (b:bs) =
      let (cout, sum) = fa cin a b in
        sum : plus cout as bs

-- | Ripple Carry Adder based negation
(-) :: Vec -> Vec -> Vec
as - bs = as + negate bs

-- | Are two vectors equal
(==) :: Vec -> Vec -> BDD
[]     == []     = true
(a:as) == (b:bs) = (a `xnor` b) && as == bs

-- | Ordering
(<=) :: Vec -> Vec -> BDD
as <= bs = (head as ==> head (bs - as))
           && (not (head as) ==> not (head (bs - as)))

-- | Right shift
(>>>) :: Vec -> Int -> Vec
bs >>> i = let sz = length bs in
  replicate (P.min i sz) false ++ take (sz P.- i) bs

-- | Left shift
(<<<) :: Int -> Vec -> Vec
i <<< bs = reverse (reverse bs >>> i)

-- | Multiply
(*) :: Vec -> Vec -> Vec
as * bs = let sz = length as in
  foldl (+) (int2vec sz 0)
    [ map (a &&) (i <<< bs) | (a, i) <- zip (reverse as) [0..] ]

-- | Element-wise disjunction of two vectors
disj :: Vec -> Vec -> Vec
disj = zipWith (||)

-- | Element-wise conjucntion of two vectors
conj :: Vec -> Vec -> Vec
conj = zipWith (&&)

-- | `if_else` of `Vec`s
ife :: BDD -> Vec -> Vec -> Vec
ife cond as bs = disj (map (cond &&) as) (map (not cond &&) bs)

-- | Is a `Vec` representing a number
-- even?
isEven :: Vec -> BDD
isEven = not . head . reverse

-- | Is a `Vec` representing a number
-- odd?
isOdd :: Vec -> BDD
isOdd = head . reverse
