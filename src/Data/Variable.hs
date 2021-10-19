module Data.Variable where

import           Prelude (Integer,
                          Show(..),
                          Num(..))

import Control.Monad (mapM)

import           Data.Bool (Bool,
                            not)
import           Data.Eq (Eq(..))
import           Data.Function ((.), flip)
import           Data.List ((++))
import qualified Data.List as List
import           Data.String (String)
import Data.Maybe (Maybe)


import           Data.Domain (Domain(..))

type VariableRef = String

data Variable a = V {
  name :: VariableRef,
  domain :: [a]
  } deriving Eq

instance (Show a) => Show (Variable a) where
  show (V n []) = n ++ " is inconsistent"
  show (V n [x]) = n ++ " = " ++ show x
  show (V n d) = n ++ "∈ {" ++ ds ++ ad ++ "}"
    where
      ds = List.intercalate "," dsl
      dsl = List.map show d'
      d' = List.take 5 d
      b = (List.length . List.take 6) d == List.length d'
      ad = if b then "" else ",..."


newUnnamedVar_ :: (Domain a) => Variable a
newUnnamedVar_ = newUnnamedVar universeAsStream

newUnnamedVar :: [a] -> Variable a
newUnnamedVar = newVar ""

newVar :: VariableRef -> [a] -> Variable a
newVar = V

newVar_ :: (Domain a) => VariableRef -> Variable a
newVar_ = flip newVar universeAsStream

filter :: (a -> Bool) -> Variable a -> Variable a
filter f v = v {domain = List.filter f (domain v)}

setVariable :: a -> Variable a -> Variable a
setVariable a v = v {domain = [a]}

isConsistent :: Variable a -> Bool
isConsistent = not . List.null . domain

isDistinct :: Variable a -> Bool
isDistinct = List.null . List.drop 1 . domain

isRef :: VariableRef -> Variable a -> Bool
isRef ref = (ref ==) . name

findVar :: VariableRef -> [Variable a] -> Maybe (Variable a)
findVar ref = List.find (isRef ref)

getVars :: [VariableRef] -> [Variable a] -> Maybe [Variable a]
getVars refs vars = mapM (`findVar` vars) refs

getRefs :: [Variable a] -> [VariableRef]
getRefs = List.map name

getDomains :: [Variable a] -> [[a]]
getDomains = List.map domain
