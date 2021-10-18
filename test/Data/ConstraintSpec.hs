module Data.ConstraintSpec where

import Test.Hspec

import Data.Constraint
import Data.Variable

main :: IO ()
main = return ()

spec :: Spec
spec = do
  describe "nothing" $ do
    it "should do nothing" $ do
      True `shouldBe` True

-- a + b = 3
cons :: Constraint Integer
cons = Constraint {constraintType=LinearEQ, factors=[], refs=["a", "b"], constant=3}

-- 2a + 3b = 0
cons2 :: Constraint Integer
cons2 = Constraint {constraintType=LinearEQ, factors=[2,3], refs=["a", "b"], constant=0}

-- 10a - b < 0
cons3 :: Constraint Integer
cons3 = Constraint {constraintType=LinearLT, factors=[10,-1], refs=["a", "b"], constant=0}

-- a && b
cons4 :: Constraint Integer
cons4 = Constraint {constraintType=BooleanConjunction, factors=[0,0], refs=["a", "b"], constant=1}

-- a && (not b)
cons5 :: Constraint Integer
cons5 = Constraint {constraintType=BooleanConjunction, factors=[0,1], refs=["a", "b"], constant=1}


vars_ :: [Variable Integer]
vars_ = [newVar_ "a", newVar_ "b"]

vars :: [Variable Integer]
vars = [newVar "a" [-5..5], newVar "b" [-5..5]]

vars2 :: [Variable Integer]
vars2 = [newVar "a" [0,1], newVar "b" [0,1]]

runSimpleConstraintTest_ :: Maybe [[Integer]]
runSimpleConstraintTest_ = listSat cons vars_

runSimpleConstraintTest :: Maybe [[Integer]]
runSimpleConstraintTest = listSat cons vars
