module Util.Stream where

import Prelude ()



(+|+) :: [a] -> [a] -> [a]
(+|+) (a:as) (b:bs) = a:b:(bs +|+ as)
(+|+) a [] = a
(+|+) [] b = b
