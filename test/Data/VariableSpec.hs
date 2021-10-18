module Data.VariableSpec where

import Test.Hspec

import Data.Variable

main :: IO ()
main = return ()

spec :: Spec
spec = do
  describe "nothing" $ do
    it "should do nothing" $ do
      True `shouldBe` True


vars_ :: [Variable Integer]
vars_ = [newVar_ "a", newVar_ "b"]

vars :: [Variable Integer]
vars = [newVar "a" [-5..5], newVar "b" [-5..5]]
