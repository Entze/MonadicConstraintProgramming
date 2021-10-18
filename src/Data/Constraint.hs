{-# LANGUAGE StandaloneDeriving #-}
module Data.Constraint where

import Prelude (Num(..),
                Show,
                Enum,
                Bounded,
                Integer,
                undefined)

import Control.Monad (Monad(..),
                      (=<<))

import Data.Bool (Bool(..), not)
import Data.Bool.Unicode
import Data.Eq (Eq(..))
import Data.Foldable (Foldable)
import Data.Function
import Data.List ((++),
                  all,
                  any,
                  and,
                  map,
                  elem,
                  zip,
                  zipWith,
                  filter,
                  sum,
                  null,
                  repeat)
import Data.Maybe (Maybe(..))
import Data.Ord (Ord(..))
import Data.Tuple (fst,
                   snd)
import Data.Tuple.Extra (dupe, second)

import Data.Variable (Variable(..),
                      VariableRef,
                      getVars,
                      getDomains)
import Util.List
import Util.Num

data Constraint a = Constraint { constraintType :: ConstraintType, factors :: [a], refs :: [VariableRef], constant :: a }

data ConstraintType = LinearEQ | LinearLT | BooleanConjunction deriving (Show, Enum, Ord, Eq, Bounded)

combineOperator LinearEQ = sum
combineOperator LinearLT = sum
combineOperator BooleanConjunction = integerAnd


compareOperator :: (Eq a, Ord a) => ConstraintType -> (a -> a -> Bool)
compareOperator LinearEQ = (==)
compareOperator LinearLT = (<)
compareOperator BooleanConjunction = (==)

factorOperator :: ConstraintType -> (Integer -> Integer -> Integer)
factorOperator LinearEQ = (*)
factorOperator LinearLT = (*)
factorOperator BooleanConjunction = (⊻!)

defaultFactor :: ConstraintType -> Integer
defaultFactor LinearEQ = 1
defaultFactor LinearLT = 1
defaultFactor BooleanConjunction = 0

deriving instance (Show a) => Show (Constraint a)
deriving instance (Eq a) => Eq (Constraint a)

isSat ::  Constraint Integer -> [Variable Integer] -> Maybe Bool
isSat cons vars = (return . not . null) =<< listSat cons vars

listSat ::  Constraint Integer -> [Variable Integer] -> Maybe [[Integer]]
listSat cons vars = do
  scaledDomains <- scaleDomains cons vars
  let c = choices scaledDomains
  return (map (map fst) (filter (flip ((compareOperator . constraintType) cons) (constant cons) . (combineOperator . constraintType) cons . map snd) c))

scaleDomains :: Constraint Integer -> [Variable Integer] -> Maybe [[(Integer, Integer)]]
scaleDomains cons vars = do
  vs <- getVars (refs cons) vars
  let domains = getDomains vs
  let domainsWithFactor = zipWith (\ds f -> map (\d -> (d, (factorOperator . constraintType) cons f d)) ds) domains (factors cons ++ repeat ((defaultFactor . constraintType) cons))
  return domainsWithFactor
