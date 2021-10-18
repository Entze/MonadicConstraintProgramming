{-# LANGUAGE StandaloneDeriving #-}
module Data.Constraint where

import Prelude (Num(..),
                Show,
                Enum,
                Bounded,
                undefined)

import Control.Monad (Monad(..))

import Data.Bool (Bool(..))
import Data.Eq (Eq(..))
import Data.Ord (Ord(..))
import Data.Maybe (Maybe(..))
import Data.List ((++),
                  all,
                  any,
                  map,
                  elem,
                  zipWith,
                  filter,
                  sum,
                  repeat)
import Data.Function

import Data.Variable (Variable(..),
                      VariableRef,
                      getVars,
                      getDomains)
import Util.Num (accumulateSums)
import Util.List

data Constraint a = Constraint { constraintType :: ConstraintType, factors :: [a], refs :: [VariableRef], constant :: a }

data ConstraintType = LinearEQ deriving (Show, Enum, Ord, Eq, Bounded)


deriving instance (Show a) => Show (Constraint a)
deriving instance (Eq a) => Eq (Constraint a)

isSat :: (Num a, Eq a) => Constraint a -> [Variable a] -> Maybe Bool
isSat cons vars
 | constraintType cons == LinearEQ = do
     scaledDomains <- scaleDomains cons vars
     let accum = accumulateSums scaledDomains
     return (constant cons `elem` accum)


listSat :: (Num a, Eq a) => Constraint a -> [Variable a] -> Maybe [[a]]
listSat cons vars = do
  scaledDomains <- scaleDomains cons vars
  let sums = choices scaledDomains
  return (filter ((== constant cons) . sum) sums)


scaleDomains :: (Num a, Eq a) => Constraint a -> [Variable a] -> Maybe [[a]]
scaleDomains cons vars = getVars (refs cons) vars >>= (return . getDomains) >>= (return . zipWith (\f d -> map (*f) d) (factors cons ++ repeat 1))
