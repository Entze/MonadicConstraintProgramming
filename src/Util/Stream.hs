module Util.Stream where

import Prelude ()



(+|+) :: [a] -> [a] -> [a]
(+|+) (a:as) (b:bs) = a:b:(as +|+ bs)
(+|+) a [] = a
(+|+) _ b = b
