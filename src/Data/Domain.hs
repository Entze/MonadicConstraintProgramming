{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Domain where

import Prelude (Enum(..),
                Bounded(..),
                Int,
                Integer)

import           Data.Bool
import           Data.Function ((.))
import           Data.List (iterate,
                            drop)
import           Data.Ord (Ord(..))
import           Data.Set (Set)
import qualified Data.Set as Set

import Util.Stream ((+|+))

class Domain a where
  universeAsStream :: [a]

instance (Ord a, Enum a, Bounded a) => Domain a where
  universeAsStream = zero:(pos zero +|+ neg zero)
    where
      zero = toEnum 0
      pos = drop 1 . iterateWhile succ (< maxBound)
      neg = drop 1 . iterateWhile pred (> minBound)
      iterateWhile :: (a -> a) -> (a -> Bool) -> a -> [a]
      iterateWhile f c x = x:if c x then iterateWhile f c (f x) else []

instance {-# OVERLAPS #-} Domain Integer where
  universeAsStream = 0:([1..] +|+ [-1,-2..])



class FiniteDomain a where
  universeAsSet :: Set a

instance (Bounded a, Enum a) => FiniteDomain a where
  universeAsSet = Set.fromDistinctAscList [minBound..maxBound]
