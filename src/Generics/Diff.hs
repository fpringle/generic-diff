{-# OPTIONS_GHC -Wno-partial-fields #-}

module Generics.Diff
  ( -- * Class
    Diff (..)

    -- ** Implementing diff
  , gdiff
  , gdiffTopLevel
  , gdiffWith
  , eqDiff
  , diffListWith

    -- * Types
  , Differ (..)
  , DiffErrorNested (..)
  , ListDiffError (..)
  , DiffError (..)
  , DiffResult (..)
  , DiffAtField (..)
  , (:*:) (..)
  )
where

import Generics.Diff.Class
import Generics.Diff.Instances ()
import Generics.Diff.Type
