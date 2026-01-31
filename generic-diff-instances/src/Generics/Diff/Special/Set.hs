{-# OPTIONS_GHC -Wno-orphans #-}

{- | A worked example of implementing 'SpecialDiff' (and thereby 'Diff') for 'Set's.

We make the choice to prioritise speed over exhaustiveness: in other words we stop when we find
one difference between the two input sets. Alternatively, we could have gone the other way and
enumerated all the difference between the inputs, using some kind of intersection test. This is left
as an exercise for the reader.

Note that the implementation for maps in "Generics.Diff.Special.Map" is very similar; this is since a
@'Set' k@ can be seen as equivalent to @Map k ()@.
-}
module Generics.Diff.Special.Set
  ( SetDiffError (..)
  )
where

import Control.Applicative ((<|>))
import Data.Set (Set)
import qualified Data.Set.Internal as Set
import Generics.Diff
import Generics.Diff.Render
import Generics.Diff.Special

-- | For 'Set's, we only pick out (maximum) one difference between the two inputs. There are two possibilities:
data SetDiffError k
  = -- | The right set contains an element that isn't found in the left set
    LeftMissingKey k
  | -- | The left set contains an element that isn't found in the right set
    RightMissingKey k
  deriving (Show, Eq)

{- | Render a 'SetDiffError'. This is a top-level function because we'll use it in the implementations
of 'renderSpecialDiffError' for both 'Set' and 'IntSet'.

There are no nested 'DiffError's here, so we use 'linesDoc'.
-}
setDiffErrorDoc :: (Show k) => SetDiffError k -> Doc
setDiffErrorDoc = \case
  LeftMissingKey k ->
    linesDoc $ pure $ "The right set contains key " <> showB k <> " but the left doesn't"
  RightMissingKey k ->
    linesDoc $ pure $ "The left set contains key " <> showB k <> " but the right doesn't"

{- | First we define an instance of 'SpecialDiff'. We need 'Show' and 'Eq' so that 'SetDiffError'
also has these instances; we need 'Ord' to compare elements of the set.
-}
instance (Show k, Eq k, Ord k) => SpecialDiff (Set k) where
  type SpecialDiffError (Set k) = SetDiffError k

  -- base cases
  specialDiff Set.Tip Set.Tip = Nothing
  specialDiff Set.Tip (Set.Bin _ k _ _) = Just $ LeftMissingKey k
  specialDiff (Set.Bin _ k _ _) Set.Tip = Just $ RightMissingKey k
  -- recursive step, using Set.split
  specialDiff (Set.Bin _ k left right) r =
    if Set.notMember k r
      then Just $ RightMissingKey k
      else
        let (less, more) = Set.split k r
        in  specialDiff left less <|> specialDiff right more

  renderSpecialDiffError = setDiffErrorDoc

-- | Now we can implement 'Diff' using 'diffWithSpecial'.
instance (Show k, Ord k) => Diff (Set k) where
  diff = diffWithSpecial
