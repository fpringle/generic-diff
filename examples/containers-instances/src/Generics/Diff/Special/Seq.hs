{-# OPTIONS_GHC -Wno-orphans #-}

-- | A worked example of implementing 'SpecialDiff' (and thereby 'Diff') for 'Seq's.
module Generics.Diff.Special.Seq () where

import Data.Foldable (toList)
import Data.Function (on)
import Data.Sequence (Seq)
import Generics.Diff
import Generics.Diff.Render
import Generics.Diff.Special

{- | Just as with the instance for lists or non-empty lists (see "Generics.Diff.Special.List"),
we can use 'ListDiffError', 'diffListWith' and 'listDiffErrorDoc'.
-}
instance (Diff a) => SpecialDiff (Seq a) where
  type SpecialDiffError (Seq a) = ListDiffError a
  specialDiff = diffListWith diff `on` toList
  renderSpecialDiffError = listDiffErrorDoc "sequence"

instance (Diff a) => Diff (Seq a) where
  diff = diffWithSpecial
