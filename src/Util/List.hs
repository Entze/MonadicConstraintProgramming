module Util.List where

import Data.List

{-
enumerate :: Int -> [[Int]]
enumerate 2 = [[0,0],[0,1],[1,0],[1,1],[0,2],[1,2],[2,1],[
enumerate 1 = map (:[]) [0..]
enumerate 0 = []
  where
    enum' (a:as) = ((a+1):as)
-}

-- | Slightly unfair 2-way Cartesian product: given two (possibly infinite)
-- lists, produce a single list such that whenever @v@ and @w@ have finite
-- indices in the input lists, @(v,w)@ has finite index in the output list.
-- Lower indices occur as the @fst@ part of the tuple more frequently, but not
-- exponentially so.
cartesianProduct :: (a -> b -> c) -> [a] -> [b] -> [c]
-- special case: don't want to construct an infinite list of empty lists to pass to diagonal
cartesianProduct _ []   _  = []
cartesianProduct f xs  ys  = diagonal [[f x y | x <- xs] | y <- ys]

-- | Slightly unfair n-way Cartesian product: given a finite number of
-- (possibly infinite) lists, produce a single list such that whenever @vi@ has
-- finite index in list i for each i, @[v1, ..., vn]@ has finite index in the
-- output list.
choices :: [[a]] -> [[a]]
choices = foldr (cartesianProduct (:)) [[]]

-- | Unfair n-way interleaving: given a possibly infinite number of (possibly
-- infinite) lists, produce a single list such that whenever @v@ has finite
-- index in an input list at finite index, @v@ also has finite index in the
-- output list. Elements from lists at lower index occur more frequently, but
-- not exponentially so.
diagonal :: [[a]] -> [a]
diagonal = concat . diagonals

-- | Like 'diagonal', but expose a tiny bit more (non-semantic) information:
-- if you lay out the input list in two dimensions, each list in the result
-- will be one of the diagonals of the input. In particular, each element of
-- the output will be a list whose elements are each from a distinct input
-- list.
diagonals :: [[a]] -> [[a]]
diagonals = tail . go [] where
 -- it is critical for some applications that we start producing answers
  -- before inspecting es_
  go b es_ = [h | h:_ <- b] : case es_ of
    []   -> transpose ts
    e:es -> go (e:ts) es
    where ts = [t | _:t <- b]
