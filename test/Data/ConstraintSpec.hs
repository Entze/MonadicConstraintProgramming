module Data.ConstraintSpec where

import Test.Hspec

import Control.Monad

import Data.List

import Data.Constraint
import Data.Variable
import Util.Instances

main :: IO ()
main = return ()

spec :: Spec
spec = do
  describe "listSat" $ do
    it "listSat cons vars == Just [[-2,-1,0,1,2,3,4,5],[-2,-1,0,1,2,3,4,5]]" $ do
      fmap sort (listSat cons vars) `shouldBe` Just [[-2,5],[-1,4],[0,3],[1,2],[2,1],[3,0],[4,-1],[5,-2]]

-- a + b = 3
cons :: Constraint Integer Integer Integer Integer
cons = linearEquals [] ["a", "b"] 3

-- 2a + 3b = 0
cons2 :: Constraint Integer Integer Integer Integer
cons2 = linearEquals [2,3] ["a", "b"] 0

-- 10a - b < 0
cons3 :: Constraint Integer Integer Integer Integer
cons3 = linearLessThan [10, -1] ["a", "b"] 0

-- a && b
cons4 :: Constraint Bool Bool Bool Bool
cons4 = booleanConjunction [] ["a", "b"]

{-
-- a && (not b)
cons5 :: Constraint Integer
cons5 = Constraint {constraintType=BooleanConjunction, factors=[0,1], refs=["a", "b"], constant=1}

-- a /= 5
cons6 :: Constraint Integer
cons6 = Constraint {constraintType=LinearNE, factors=[], refs=["a"], constant=5}
-}

vars_ :: [Variable Integer]
vars_ = [newVar_ "a", newVar_ "b"]

vars :: [Variable Integer]
vars = [newVar "a" [-5..5], newVar "b" [-5..5]]

varsBool :: [Variable Bool]
varsBool = [newVar_ "a", newVar_ "b" ]

runSimpleConstraintTest_ :: Maybe [[Integer]]
runSimpleConstraintTest_ = listSat cons vars_

runSimpleConstraintTest :: Maybe [[Integer]]
runSimpleConstraintTest = listSat cons vars
