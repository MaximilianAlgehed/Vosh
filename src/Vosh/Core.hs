{-# LANGUAGE NoImplicitPrelude #-}
module Vosh.Core where

import qualified Cudd.Imperative as IMP
import           Cudd.Cudd
import           Cudd.Convert

import           System.IO
import           System.Process

import           Control.Monad
import           Control.Monad.ST

import           Data.Bool (Bool, otherwise)
import           Data.Int
import           Data.List

import           Vosh.Class
import           Vosh.Prelude

-- | Binary Decision Diagrams
newtype BDD = BDD { runBDD :: DDManager -> DDNode }

-- | Embed a nullary operation from `CUDD`
nullary :: (DDManager -> DDNode) -> BDD
nullary = BDD

-- | Embed a unary operation from `CUDD`
unary :: (DDManager -> DDNode -> DDNode) -> BDD -> BDD
unary foo bdd = BDD $ \m -> foo m (runBDD bdd m)

-- | Embed a binary operation from `CUDD`
binary :: (DDManager -> DDNode -> DDNode -> DDNode) -> BDD -> BDD -> BDD
binary op f g = BDD $ \m -> op m (runBDD f m) (runBDD g m)

-- | BDDs are Booleans
instance Boolean BDD where
  true  = nullary readOne 
  false = nullary readLogicZero
  (&&)  = binary  bAnd
  (||)  = binary  bOr
  not   = unary   bNot
  xor   = binary  bXor
  xnor  = binary  bXnor

-- | Create a BDD variable
v :: Int -> BDD
v i = BDD $ \m -> ithVar m i

-- | Existential quantification
exists :: BDD -> BDD -> BDD
exists = flip $ binary bExists

-- | Existential quantification over a list of BDDs
existsMany :: [BDD] -> BDD -> BDD
existsMany = flip $ foldl exists

-- | Universal quantification
forall :: BDD -> BDD -> BDD
forall = flip $ binary bForall

-- | Substitute a variable for a BDD
substitute :: (Int, BDD) -> BDD -> BDD
substitute (idx, bdd') bdd =
  BDD $ \m -> runST $ do
    let impM = toImperativeManager m
    impbdd  <- toImperativeNode (runBDD bdd m)
    impbdd' <- toImperativeNode (runBDD bdd' m)
    fromImperativeNode m <$> IMP.compose impM impbdd impbdd' idx

-- | Evaluate a BDD expression
evaluate :: DDManager -> BDD -> [Bool] -> Bool
evaluate m bdd ass = eval m (runBDD bdd m) [ if a then 1 else 0 | a <- ass]

-- | Display a BDD
-- TODO: Use `dumpDot'` to avoid an intermidiary file
display :: DDManager -> BDD -> IO ()
display m bdd = do
  dumpDot m (runBDD bdd m) "/tmp/graph.dot"
  callCommand "dot -Txlib /tmp/graph.dot"

-- | New names for `DDManager`s
defaults :: DDManager
defaults = cuddInit

-- | New names for `DDManager`s
ordered :: [Int] -> DDManager
ordered = cuddInitOrder
