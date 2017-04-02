module Vosh where

import qualified Prelude as Meta
import Prelude (($))
import System.IO
import Data.Int
import Data.Bool (Bool, otherwise)
import qualified Data.Bool as Bool
import Cudd.Cudd
import System.Process

newtype BDD = BDD { runBDD :: DDManager -> DDNode }

class Boolean a where
  true, false :: a
  false = not true
  (&&) :: a -> a -> a
  not  :: a -> a
  (||) :: a -> a -> a
  a || b = not (not a && not b)

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
