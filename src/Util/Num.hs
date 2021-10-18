module Util.Num where

accumulateSums :: (Num a) => [[a]] -> [a]
accumulateSums [v] = v
accumulateSums (v:vs) = [ n + a | n <- v, a <- accumulated]
  where
    accumulated = accumulateSums vs
accumulateSums [] = []
