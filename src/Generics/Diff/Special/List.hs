{- | Diffs on lists as a special case. See "Generics.Diff.Special" for a detailed explanation
of the implementation.
-}
module Generics.Diff.Special.List
  ( ListDiffError (..)
  , diffListWith
  )
where

import Generics.Diff.Class
import Generics.Diff.Type
