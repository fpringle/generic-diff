{-# OPTIONS_GHC -Wno-orphans #-}

module Generics.Diff.Instances where

import qualified Data.List.NonEmpty as NE
import Generics.Diff.Class
import Generics.Diff.Type

instance Diff Int where diff = eqDiff

instance Diff Bool where diff = eqDiff

instance Diff () where diff () () = Equal

instance (Diff a, Diff b) => Diff (Either a b)

instance (Diff a) => Diff (Maybe a)

instance (Diff a) => Diff [a] where
  {-# SPECIALIZE instance Diff [Char] #-}
  diff = diffList

instance Diff Char where
  diff = eqDiff
  diffList = eqDiff

instance (Diff a) => Diff (NE.NonEmpty a) where
  diff l r = diffListWith DiffNonEmpty diff (NE.toList l) (NE.toList r)
