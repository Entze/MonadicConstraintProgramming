module Data.ConstraintTree where

import Prelude (Integer)

import Control.Monad (Monad(..),
                      (>=>),
                      ap,
                      liftM)
import Control.Applicative (Applicative(..))

import Data.Functor (Functor(..))
import Data.List ()

import Data.Variable
import Data.Constraint

data ConstraintTree solver result
  = Res result
  | Var (Variable Integer -> ConstraintTree solver result)
  | Con (Constraint Integer) (ConstraintTree solver result)
  | Bra (ConstraintTree solver result) (ConstraintTree solver result)
  | Inc
  | Dyn (solver (ConstraintTree solver result))

instance (Monad solver) => Functor (ConstraintTree solver) where
  fmap = liftM

instance (Monad solver) => Applicative (ConstraintTree solver) where
  pure = return
  (<*>) = ap

instance (Monad solver) => Monad (ConstraintTree solver) where
  return = Res
  (Con c s) >>= f = Con c (s >>= f)
  (Res r) >>= f = f r
  (Var f') >>= f = Var (f' >=> f)
  (Bra l r) >>= f = Bra (l >>= f) (r >>= f)
  (Dyn s) >>= f = Dyn s >>= f
  Inc >>= _ = Inc
