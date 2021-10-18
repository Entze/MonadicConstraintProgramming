module Util.Num where

integerAnd :: (Foldable t) => t Integer -> Integer
integerAnd = fromIntegral . fromEnum . all (== 1)

integerOr :: (Foldable t) => t Integer -> Integer
integerOr = fromIntegral . fromEnum . elem 1

(⊻!) :: Integer -> Integer -> Integer
(⊻!) 0 0 = 0
(⊻!) 0 _ = 1
(⊻!) _ 0 = 1
(⊻!) _ _ = 0
