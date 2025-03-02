{-# LANGUAGE QuantifiedConstraints #-}

module Diff.Generic where

import Data.SOP
import Generics.SOP

newtype MyNP f xs = MyNP (NP f xs)

deriving instance (All (Compose Show f) xs) => Show (MyNP f xs)

newtype MyNP2 f xs = MyNP2 (NP FieldInfo xs)

instance (forall x. Show (f x)) => Show (MyNP2 f xs)

data NP' f xs where
  Nil :: NP' f '[]
  Cons :: f x -> NP' f xs -> NP' f (x ': xs)

deriving instance (forall x. Show (f x)) => Show (NP' f xs)
