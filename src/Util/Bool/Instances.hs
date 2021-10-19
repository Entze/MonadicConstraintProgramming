{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Util.Bool.Instances where

import Control.Applicative

import Data.Monoid (Monoid(..),
                   Product(..),
                   Sum(..))
import Data.Semigroup (Semigroup(..))
import Data.Bits

type Conj = Product Bool
deriving instance Enum Conj
deriving instance Bits Conj
deriving instance FiniteBits Conj

type Disj = Sum Bool
deriving instance Enum Disj

instance {-# OVERLAPS #-} Semigroup Conj where
  (<>) = liftA2 (&&)

instance {-# OVERLAPS #-} Monoid Conj where
  mempty = pure True



instance {-# OVERLAPS #-} Semigroup Disj where
  (<>) = liftA2 (&&)

instance {-# OVERLAPS #-} Monoid Disj where
  mempty = pure True
