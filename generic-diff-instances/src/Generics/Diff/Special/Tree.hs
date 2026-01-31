{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | A worked example of implementing 'SpecialDiff' (and thereby 'Diff') for 'Tree's.

As with other 3rd-party types, there are different approaches we can take here. We'll show 2 of them:

- using 'gspecialDiffNested';
- using 'SpecialDiff' and a custom diff type.
-}
module Generics.Diff.Special.Tree where

import Control.Applicative
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Tree as Tree
import Generics.Diff
import Generics.Diff.Render
import Generics.Diff.Special
import Generics.SOP.GGP

------------------------------------------------------------
-- Using gspecialDiffNested

instance (Diff a) => SpecialDiff (Tree.Tree a) where
  type SpecialDiffError (Tree.Tree a) = DiffErrorNested (GCode (Tree.Tree a))
  specialDiff = gspecialDiffNested
  renderSpecialDiffError = diffErrorNestedDoc

instance (Diff a) => Diff (Tree.Tree a) where
  diff = diffWithSpecial

------------------------------------------------------------
-- Using SpecialDiff

newtype CustomTree a = CustomTree (Tree.Tree a)
  deriving (Show) via (Tree.Tree a)

newtype TreePath = TreePath [Int]
  deriving (Show, Eq) via [Int]

data CustomTreeDiffError a
  = DiffAtNode TreePath (DiffError a)
  | WrongLengthsOfChildren TreePath Int Int
  deriving (Show, Eq)

renderTreePath :: TreePath -> TB.Builder
renderTreePath (TreePath []) = "<root>"
renderTreePath (TreePath (x : xs)) = mconcat $ showB x : ["->" <> showB y | y <- xs]

instance (Diff a) => SpecialDiff (CustomTree a) where
  type SpecialDiffError (CustomTree a) = CustomTreeDiffError a

  renderSpecialDiffError = \case
    DiffAtNode path err ->
      let ls = pure $ "Diff between nodes at path " <> renderTreePath path
      in  makeDoc ls err
    WrongLengthsOfChildren path l r ->
      let ls =
            ("Child lists at path " <> renderTreePath path <> " are wrong lengths")
              :| [ "Length of left child list: " <> showB l
                 , "Length of right child list: " <> showB r
                 ]
      in  linesDoc ls

  specialDiff (CustomTree l) (CustomTree r) = go [] l r
    where
      go curPath (Tree.Node n1 f1) (Tree.Node n2 f2) =
        case diff n1 n2 of
          Error err -> Just $ DiffAtNode curTreePath err
          Equal ->
            let go' n = go (n : curPath)
                goChildren _ [] [] = Nothing
                goChildren n [] ys = Just $ WrongLengthsOfChildren curTreePath n (n + length ys)
                goChildren n xs [] = Just $ WrongLengthsOfChildren curTreePath (n + length xs) n
                goChildren n (x : xs) (y : ys) = go' n x y <|> goChildren (n + 1) xs ys
            in  goChildren 0 f1 f2
        where
          curTreePath = TreePath $ reverse curPath

instance (Diff a) => Diff (CustomTree a) where
  diff = diffWithSpecial
