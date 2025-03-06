{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Generics.Diff.Class
  ( -- * Class
    Diff (..)

    -- ** Implementing diff
  , gdiff
  , gdiffTopLevel
  , gdiffWith
  , eqDiff
  , diffListWith
  )
where

import Data.SOP
import Data.SOP.NP
import Generics.Diff.Type
import Generics.SOP

shiftAtLoc :: AtLoc f xs -> AtLoc f (x ': xs)
shiftAtLoc (AtLoc ns) = AtLoc (S ns)

shiftDiffError :: DiffErrorNested xs -> DiffErrorNested (x ': xs)
shiftDiffError = \case
  TopLevelNotEqual -> TopLevelNotEqual
  WrongConstructor xs ys -> WrongConstructor (S xs) (S ys)
  FieldMismatch atLoc -> FieldMismatch (shiftAtLoc atLoc)

class Diff a where
  diff :: a -> a -> DiffResult a
  default diff :: (Generic a, HasDatatypeInfo a, All2 Diff (Code a)) => a -> a -> DiffResult a
  diff = gdiff

  diffList :: [a] -> [a] -> DiffResult [a]
  diffList = diffListWith diff

diffListWith :: (a -> a -> DiffResult a) -> [a] -> [a] -> DiffResult [a]
diffListWith d = go 0
  where
    go _ [] [] = Equal
    go n [] ys = Error $ DiffList $ WrongLengths n (n + length ys)
    go n xs [] = Error $ DiffList $ WrongLengths (n + length xs) n
    go n (x : xs) (y : ys) = case d x y of
      Equal -> go (n + 1) xs ys
      Error err -> Error $ DiffList $ DiffAtIndex n err

eqDiff :: (Eq a) => a -> a -> DiffResult a
eqDiff a b =
  if a == b
    then Equal
    else Error $ Nested TopLevelNotEqual

eqDiffer :: (Eq a) => Differ a
eqDiffer = Differ eqDiff

gdiff ::
  forall a.
  (Generic a, HasDatatypeInfo a, All2 Diff (Code a)) =>
  a ->
  a ->
  DiffResult a
gdiff = gdiffWith $ cpure_POP (Proxy @Diff) (Differ diff)

gdiffTopLevel ::
  forall a.
  (Generic a, HasDatatypeInfo a, All2 Eq (Code a)) =>
  a ->
  a ->
  DiffResult a
gdiffTopLevel = gdiffWith $ cpure_POP (Proxy @Eq) eqDiffer

gdiffWith ::
  forall a.
  (Generic a, HasDatatypeInfo a) =>
  POP Differ (Code a) ->
  a ->
  a ->
  DiffResult a
gdiffWith (POP ds) (from -> SOP xs) (from -> SOP ys) =
  maybe Equal (Error . Nested) $ gdiff' (constructorInfo $ datatypeInfo $ Proxy @a) ds xs ys

gdiff' ::
  NP ConstructorInfo xss ->
  NP (NP Differ) xss ->
  NS (NP I) xss ->
  NS (NP I) xss ->
  Maybe (DiffErrorNested xss)
gdiff' _ Nil xss _ = case xss of {}
gdiff' (i :* _) (ds :* _) (Z xs) (Z ys) =
  FieldMismatch . AtLoc . Z . (i :*:) <$> goProduct ds xs ys
  where
    goProduct :: forall as. NP Differ as -> NP I as -> NP I as -> Maybe (NS DiffError as)
    goProduct Nil Nil Nil = Nothing
    goProduct (Differ d :* ds') (I x :* bs) (I y :* cs) =
      case d x y of
        Equal -> S <$> goProduct ds' bs cs
        Error err -> Just $ Z err
gdiff' (i :* is) _ (Z _) (S rest) = Just $ WrongConstructor (Z i) (S $ pickOut is rest)
gdiff' (i :* is) _ (S rest) (Z _) = Just $ WrongConstructor (S $ pickOut is rest) (Z i)
gdiff' (_ :* is) (_ :* dss) (S xs) (S ys) = shiftDiffError <$> gdiff' is dss xs ys
