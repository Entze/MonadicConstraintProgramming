module Util.Bool where

generalAnd :: (Foldable t, Eq n, Monoid n, Enum n) => t n -> n
generalAnd = toEnum . fromEnum . all (== succ mempty)

generalOr :: (Foldable t, Eq n, Monoid n, Enum n) => t n -> n
generalOr = toEnum . fromEnum . any (/= mempty)

(⊻!) :: (Eq n, Enum n, Monoid n) => n -> n -> n
(⊻!) a b = (toEnum . fromEnum) ((a' && not b') || (not a' && b'))
  where
    a' = a == mempty
    b' = b == mempty
