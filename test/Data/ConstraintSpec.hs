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

cons :: Constraint Integer
cons = Constraint {constraintType=LinearEQ, factors=[], refs=["a", "b"], constant=3}

vars_ :: [Variable Integer]
vars_ = [newVar_ "a", newVar_ "b"]

vars :: [Variable Integer]
vars = [newVar "a" [-5..5], newVar "b" [-5..5]]

runSimpleConstraintTest_ :: Maybe [[Integer]]
runSimpleConstraintTest_ = listSat cons vars_

runSimpleConstraintTest :: Maybe [[Integer]]
runSimpleConstraintTest = listSat cons vars
