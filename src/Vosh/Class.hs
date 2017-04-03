{-# LANGUAGE NoImplicitPrelude #-}
module Vosh.Class where

import qualified Data.Bool as Bool
import           Data.Bool    (Bool, otherwise)

import           Vosh.Prelude

-- | Relevant operations on booleans
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

-- | Make sure `Boolean` operations
-- have the same fixity as the operations
-- from `Data.Bool`
infixr 3 &&
infixr 2 ||
infixr 1 ==>

-- | Bools are Booleans
instance Boolean Bool where
  true  = Bool.True
  false = Bool.False
  (&&)  = (Bool.&&)
  (||)  = (Bool.||)
  not   = Bool.not
