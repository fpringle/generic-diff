{-# OPTIONS_GHC -Wno-orphans #-}

{- | A worked example of implementing 'SpecialDiff' (and thereby 'Diff') for 'Map's.

We make the choice to prioritise speed over exhaustiveness: in other words we stop when we find
one difference between the two input maps. Alternatively, we could have gone the other way and
enumerated all the difference between the inputs, using some kind of intersection test. This is left
as an exercise for the reader.
-}
module Generics.Diff.Special.Map
  ( MapDiffError (..)
  )
where

import Control.Applicative ((<|>))
import Data.Map (Map)
import qualified Data.Map.Internal as Map
import Generics.Diff
import Generics.Diff.Render
import Generics.Diff.Special

-- | For 'Map's, we only pick out (maximum) one difference between the two inputs. There are three possibilities:
data MapDiffError k v
  = -- | A key is found in both maps, but they have different values.
    DiffAtKey k (DiffError v)
  | -- | The right set contains an element that isn't found in the left set
    LeftMissingKey k
  | -- | The left set contains an element that isn't found in the right set
    RightMissingKey k
  deriving (Show, Eq)

{- | Render a 'MapDiffError'. This is a top-level function because we'll use it in the implementations
of 'renderSpecialDiffError' for both 'Map' and 'Data.IntMap.IntMap'.
-}
mapDiffErrorDoc :: (Show k) => MapDiffError k v -> Doc
mapDiffErrorDoc = \case
  -- Since we have a nested 'DiffError' on the value, we use 'makeDoc'.
  DiffAtKey k err ->
    let lns = pure ("Both maps contain key " <> showB k <> " but the values differ:")
    in  makeDoc lns err
  LeftMissingKey k ->
    linesDoc $ pure $ "The right map contains key " <> showB k <> " but the left doesn't"
  RightMissingKey k ->
    linesDoc $ pure $ "The left map contains key " <> showB k <> " but the right doesn't"

instance (Show k, Ord k, Diff v) => SpecialDiff (Map k v) where
  type SpecialDiffError (Map k v) = MapDiffError k v

  -- base cases
  specialDiff Map.Tip Map.Tip = Nothing
  specialDiff Map.Tip (Map.Bin _ k _ _ _) = Just $ LeftMissingKey k
  specialDiff (Map.Bin _ k _ _ _) Map.Tip = Just $ RightMissingKey k
  -- recursive set, using Map.split
  specialDiff (Map.Bin _ k lVal left right) r = case Map.lookup k r of
    Nothing -> Just $ RightMissingKey k
    Just rVal ->
      -- first we check if the values are different (using the 'Diff' instance on v)
      case diff lVal rVal of
        Error err -> Just $ DiffAtKey k err
        Equal ->
          -- otherwise, split and recurse
          let (less, more) = Map.split k r
          in  specialDiff left less <|> specialDiff right more

  renderSpecialDiffError = mapDiffErrorDoc

instance (Show k, Ord k, Diff v) => Diff (Map k v) where
  diff = diffWithSpecial
