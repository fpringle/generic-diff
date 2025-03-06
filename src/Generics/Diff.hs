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
  , AtLoc (..)
  , (:*:) (..)

    -- * Delete me: for testing
  , Test (..)
  , drActs
  , drExps
  )
where

import Data.SOP
import qualified GHC.Generics as G
import Generics.Diff.Class
import Generics.Diff.Instances ()
import Generics.Diff.Type
import Generics.SOP

------------------------------------------------------------
-- test type

data Test
  = C1 (Either Int String) (Maybe Char)
  | C2 {c11 :: Maybe String, c12 :: Bool}
  deriving (Show, Eq, G.Generic)

instance Generic Test

instance HasDatatypeInfo Test

c1Info :: ConstructorInfo [Either Int String, Maybe Char]
c2Info :: ConstructorInfo [Maybe String, Bool]
c1Info :* c2Info :* _ = constructorInfo $ datatypeInfo $ Proxy @Test

drExps :: [(Test, Test, DiffResult Test)]
drExps =
  [
    ( C1 (Left 0) Nothing
    , C1 (Left 0) Nothing
    , Equal
    )
  ,
    ( C1 (Left 0) Nothing
    , C1 (Left 0) (Just 'a')
    , Error (Nested $ FieldMismatch (AtLoc (Z (Constructor "C1" :*: S (Z $ Nested TopLevelNotEqual)))))
    )
  ,
    ( C1 (Left 0) Nothing
    , C2 Nothing False
    , Error (Nested $ WrongConstructor (Z c1Info) (S (Z c2Info)))
    )
  ]

drActs :: [(Test, Test, DiffResult Test)]
drActs = [(t1, t2, gdiffTopLevel t1 t2) | (t1, t2, _) <- drExps]
