module Data.Constraint where

import Prelude (Num(..),
                Show,
                Bounded,
                Integer,
                Enum(..),
                Show(..),
                undefined)

import Control.Monad (Monad(..),
                      (=<<),
                      mapM,
                      liftM)

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
                  transpose,
                  intercalate,
                  foldr,
                  filter,
                  sum,
                  null,
                  repeat)
import Data.Maybe (Maybe(..))
import Data.Monoid (Monoid(..))
import Data.Ord (Ord(..))
import Data.Semigroup (Semigroup (..))
import Data.String (String(..))
import Data.Tuple (fst,
                   snd)
import Data.Tuple.Extra (dupe, second)

import Data.Variable (Variable(..),
                      VariableRef,
                      getVars,
                      getDomains)
import Util.List
import Util.Bool ((⊻!),
                  generalAnd)

data Constraint fac var res con = Constraint {factors :: [fac],
                                              refs :: [VariableRef],
                                              constant :: con,
                                              combineOp :: [res] -> res,
                                              compareOp :: res -> con -> Bool,
                                              factorOp :: fac -> var -> res,
                                              defaultFactor :: fac}

generalRelation :: (Monoid fac, Monoid res, Monoid con) => [fac] -> [VariableRef] -> (fac -> var -> res) -> (res -> con -> Bool) -> Constraint fac var res con
generalRelation fs rs factOp compOp = Constraint {factors = fs,
                                         refs = rs,
                                         constant = mempty,
                                         combineOp = mconcat,
                                         compareOp = compOp,
                                         factorOp = factOp,
                                         defaultFactor = mempty
                                        }

specializedRelation :: (Monoid n, Monoid n, Monoid n) => [n] -> [VariableRef] -> (n -> n -> Bool) -> Constraint n n n n
specializedRelation fs rs compOp = Constraint {factors = fs,
                                        refs = rs,
                                        constant = mempty,
                                        combineOp = mconcat,
                                        compareOp = compOp,
                                        factorOp = (<>),
                                        defaultFactor = mempty
                                         }


linearEquals :: (Num n, Eq n) => [n] -> [VariableRef] -> n -> Constraint n n n n
linearEquals fs rs cs = Constraint {factors = fs,
                                    refs = rs,
                                    constant = cs,
                                    combineOp = sum,
                                    compareOp = (==),
                                    factorOp = (*),
                                    defaultFactor = 1}

linearNotEquals :: (Num n, Eq n) => [n] -> [VariableRef] -> n -> Constraint n n n n
linearNotEquals fs rs cs = Constraint {factors = fs,
                                       refs = rs,
                                       constant = cs,
                                       combineOp = sum,
                                       compareOp = (/=),
                                       factorOp = (*),
                                       defaultFactor = 1}

linearLessThan :: (Num n, Eq n, Ord n) => [n] -> [VariableRef] -> n -> Constraint n n n n
linearLessThan fs rs cs = Constraint {factors = fs,
                                      refs = rs,
                                      constant = cs,
                                      combineOp = sum,
                                      compareOp = (<),
                                      factorOp = (*),
                                      defaultFactor = 1}

booleanConjunction :: [Bool] -> [VariableRef] -> Constraint Bool Bool Bool Bool
booleanConjunction fs rs = Constraint {factors = fs,
                                       refs = rs,
                                       constant = True,
                                       combineOp = and,
                                       compareOp = (==),
                                       factorOp = (⊻),
                                       defaultFactor = False}



prettyConstraint :: (Show factor, Show constant) => Constraint factor v r constant -> String -> String -> String -> String
prettyConstraint constraint factorOpString combineOpString compareOpString = intercalate combineOpString
  (zipWith (\a b -> a ++ factorOpString ++ b) (map show factors') refs') ++ compareOpString ++ show constant'
  where
    refs'     = refs      constraint
    constant' = constant  constraint
    factors'  = factors   constraint ++ (repeat . defaultFactor) constraint


isSat ::  Constraint factor vars res cons -> [Variable vars] -> Maybe Bool
isSat cons vars = (return . not . null) =<< listSat cons vars

listSat :: Constraint fac var res con -> [Variable var] -> Maybe [[var]]
listSat constraint vars = do
  fs <- getFactored constraint vars
  let fs' = choices fs
  return (map (map fst) (filter ((`compare'` constant') . combine' . map snd) fs'))
  where
    constant' = constant  constraint
    combine'  = combineOp constraint
    compare'  = compareOp constraint

getFactored :: Constraint fac var res con -> [Variable var] -> Maybe [[(var, res)]]
getFactored constraint vars = do
  vs <- (flip getVars vars . refs) constraint
  let domains = getDomains vs
  return (zipWith (\f ds -> map (\d -> (d, f `factor'` d)) ds) factors' domains)
  where
    factors'  = factors   constraint ++ (repeat . defaultFactor) constraint
    factor'   = factorOp  constraint

refineDomains :: Constraint fac var res con -> [Variable var] -> Maybe [Variable var]
refineDomains cons vars = do
  ds <- listSat cons vars
  return (zipWith (\v d -> v {domain=d}) vars (transpose ds))
