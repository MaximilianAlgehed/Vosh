{-# LANGUAGE NoImplicitPrelude, ExistentialQuantification, RankNTypes #-}
module Vosh.Core where

import Cudd.Imperative
import Cudd.Convert
import Cudd.Reorder
import qualified Cudd.Cudd as Cudd

import System.IO
import System.Process

import Control.Monad
import Control.Monad.ST

import Data.Bool (Bool, otherwise)
import Data.Int
import Data.List

import Vosh.Class
import Vosh.Prelude

-- | Binary Decision Diagrams
data BDDInternal s u = BDDInternal { runBDDInternal :: DDManager s u -> Node s u }

data BDD = forall s. BDD (BDDInternal s ())

-- | Nodes in the BDDInternal
type Node s u = ST s (DDNode s u)

-- | Embed a nullary operation from `CUDD`
nullary :: (DDManager s u -> Node s u) -> BDDInternal s u
nullary op = BDDInternal $ \m -> op m

-- | Embed a unary operation from `CUDD`
unary :: (DDManager s u -> DDNode s u -> Node s u) -> BDDInternal s u -> BDDInternal s u
unary op l = BDDInternal $ \m -> join (op m <$> (runBDDInternal l m))

-- | Embed a binary operation from `CUDD`
binary :: (DDManager s u -> DDNode s u -> DDNode s u -> Node s u) -> BDDInternal s u -> BDDInternal s u -> BDDInternal s u
binary op l r = BDDInternal $ \m -> join (op m <$> runBDDInternal l m <*> runBDDInternal r m)

-- | BDDInternals are Booleans
instance Boolean (BDDInternal s u) where
  true    = nullary (return . bOne)
  false   = nullary (return . bZero)
  (&&)    = binary  bAnd
  (||)    = binary  bOr
  not bdd = BDDInternal $ \m -> bNot <$> runBDDInternal bdd m
  xor     = binary  bXor
  xnor    = binary  bXnor

-- | BDD's are Booleans
instance Boolean BDD where
  true  = BDD true
  false = BDD false
  (&&)  = \(BDD l) (BDD r) -> BDD $ l && r
  (||)  = \(BDD l) (BDD r) -> BDD $ l || r
  not   = undefined
  xor   = undefined
  xnor  = undefined 

-- | Create a BDDInternal variable
v :: Int -> BDD
v i = BDD $ BDDInternal $ \m -> ithVar m i

-- | Existential quantification
exists :: BDDInternal s u -> BDDInternal s u -> BDDInternal s u
exists = flip $ binary bExists

-- | Existential quantification over a list of variables
existsMany :: [Int] -> BDDInternal s u -> BDDInternal s u
existsMany vars = exists (BDDInternal $ \m -> indicesToCube m vars)

-- | Universal quantification
forall :: BDDInternal s u -> BDDInternal s u -> BDDInternal s u
forall = flip $ binary bForall

-- | Substitute a variable for a BDDInternal
substitute :: [(Int, BDDInternal s u)] -> BDDInternal s u -> BDDInternal s u
substitute = flip $ foldl go
  where
    go :: BDDInternal s u -> (Int, BDDInternal s u) -> BDDInternal s u
    go bdd (idx, bdd') =
      BDDInternal $ \m -> do
        impbdd  <- runBDDInternal bdd m
        impbdd' <- runBDDInternal bdd' m
        compose m impbdd impbdd' idx

{-
-- | Evaluate a BDDInternal expression
evaluate :: Manager -> BDDInternal -> [Bool] -> Bool
evaluate m bdd ass = eval m (runBDDInternal bdd m) [ if a then 1 else 0 | a <- ass]
-}


-- | Display a BDDInternal
-- TODO: Use `dumpDot'` to avoid an intermidiary file
display :: (forall s. ST s (DDManager s u)) -> (forall s. BDDInternal s u) -> IO ()
display stm bdd = do
  (m', bdd') <- return $ runST $ do
    m       <- stm
    bdd'    <- runBDDInternal bdd m
    return (fromImperativeManager m, fromImperativeNode (fromImperativeManager m) bdd')
  Cudd.dumpDot m' bdd' "/tmp/graph.dot"
  callCommand "dot -Txlib /tmp/graph.dot"

-- | New names for `DDManager`s
defaults :: ST s (DDManager s u)
defaults = cuddInitDefaults

-- | New names for `DDManager`s
ordered :: [Int] -> ST s (DDManager s u)
ordered ord = do
  manager <- defaults
  shuffleHeap manager ord
  return manager

-- | This instance is missing from cudd 
instance Show SatBit where
  show Zero     = "0"
  show One      = "1"
  show DontCare = "-"

{-
-- | Find everything that satisfies a BDDInternal
sats :: DDManager s u -> BDDInternal -> [[SatBit]]
sats m bdd = allSat m (runBDDInternal bdd m) 
-}
