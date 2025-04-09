{-# OPTIONS_GHC -Wno-orphans #-}

{- | Diffs on lists as a special case. See "Generics.Diff.Special" for a detailed explanation
of the implementation.
-}
module Generics.Diff.Special.List
  ( ListDiffError (..)
  , diffListWith
  )
where

import Data.Function (on)
import qualified Data.List.NonEmpty as NE
import Generics.Diff.Class
import Generics.Diff.Render
import Generics.Diff.Type

instance (Diff a) => SpecialDiff (NE.NonEmpty a) where
  type SpecialDiffError (NE.NonEmpty a) = ListDiffError a
  specialDiff = diffListWith diff `on` NE.toList
  renderSpecialDiffError = listDiffErrorDoc "non-empty list"
