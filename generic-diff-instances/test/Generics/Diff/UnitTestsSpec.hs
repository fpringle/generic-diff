{-# OPTIONS_GHC -Wno-partial-fields #-}

module Generics.Diff.UnitTestsSpec where

import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Tree (Tree)
import qualified Data.Tree as Tree
import Generics.Diff
import Generics.Diff.Instances ()
import Generics.Diff.Special.Map as Map
import Generics.Diff.Special.Seq ()
import Generics.Diff.Special.Set as Set
import Generics.Diff.Special.Tree
import Generics.SOP
import Generics.SOP.GGP
import qualified Test.Hspec as H
import qualified Test.Hspec.QuickCheck as H
import Util

spec :: H.Spec
spec =
  H.describe "Unit tests" $ do
    H.describe "Map" $ traverse_ specTestSet mapTestSets
    H.describe "Set" $ traverse_ specTestSet setTestSets
    H.describe "Seq" $ traverse_ specTestSet seqTestSets
    H.describe "Tree" $ traverse_ specTestSet treeTestSets
    H.describe "CustomTree" $ traverse_ specTestSet customTreeTestSets

specTestSet :: (Diff a, Show a) => TestSet a -> H.Spec
specTestSet TestSet {..} =
  H.prop (T.unpack setName) $
    propDiffResult leftValue rightValue expectedDiffResult

data TestSet a = TestSet
  { setName :: T.Text
  , leftValue :: a
  , rightValue :: a
  , expectedDiffResult :: DiffResult a
  }
  deriving (Show)

setTestSets :: [TestSet (Set Int)]
setTestSets =
  [ TestSet
      { setName = "Equal"
      , leftValue = value1
      , rightValue = value1
      , expectedDiffResult = Equal
      }
  , TestSet
      { setName = "Diff, LeftMissingKey"
      , leftValue = value1
      , rightValue = value2
      , expectedDiffResult = Error error2
      }
  , TestSet
      { setName = "Diff, RightMissingKey"
      , leftValue = value1
      , rightValue = value3
      , expectedDiffResult = Error error3
      }
  ]
  where
    value1 = Set.fromList [1, 3]

    value2 = Set.fromList [1, 2, 3]
    error2 = DiffSpecial $ Set.LeftMissingKey 2

    value3 = Set.fromList [1]
    error3 = DiffSpecial $ Set.RightMissingKey 3

mapTestSets :: [TestSet (Map Int String)]
mapTestSets =
  [ TestSet
      { setName = "Equal"
      , leftValue = value1
      , rightValue = value1
      , expectedDiffResult = Equal
      }
  , TestSet
      { setName = "Diff, DiffAtKey"
      , leftValue = value1
      , rightValue = value2
      , expectedDiffResult = Error error2
      }
  , TestSet
      { setName = "Diff, LeftMissingKey"
      , leftValue = value1
      , rightValue = value3
      , expectedDiffResult = Error error3
      }
  , TestSet
      { setName = "Diff, RightMissingKey"
      , leftValue = value1
      , rightValue = value4
      , expectedDiffResult = Error error4
      }
  ]
  where
    value1 = Map.fromList [(1, "one"), (3, "three")]

    value2 = Map.fromList [(1, "one"), (3, "THREE")]
    error2 = DiffSpecial $ Map.DiffAtKey 3 TopLevelNotEqual

    value3 = Map.fromList [(1, "one"), (2, "two"), (3, "three")]
    error3 = DiffSpecial $ Map.LeftMissingKey 2

    value4 = Map.fromList [(1, "one")]
    error4 = DiffSpecial $ Map.RightMissingKey 3

seqTestSets :: [TestSet (Seq Int)]
seqTestSets =
  [ TestSet
      { setName = "Equal"
      , leftValue = value1
      , rightValue = value1
      , expectedDiffResult = Equal
      }
  , TestSet
      { setName = "Diff, WrongLengths"
      , leftValue = value1
      , rightValue = value2
      , expectedDiffResult = Error error2
      }
  , TestSet
      { setName = "Diff, DiffAtIndex"
      , leftValue = value1
      , rightValue = value3
      , expectedDiffResult = Error error3
      }
  ]
  where
    value1 = Seq.fromList [1, 3]

    value2 = Seq.fromList [1, 3, 4]
    error2 = DiffSpecial $ WrongLengths 2 3

    value3 = Seq.fromList [1, 2]
    error3 = DiffSpecial $ DiffAtIndex 1 TopLevelNotEqual

treeTestSets :: [TestSet (Tree Int)]
treeTestSets =
  [ TestSet
      { setName = "Equal"
      , leftValue = value1
      , rightValue = value1
      , expectedDiffResult = Equal
      }
  , TestSet
      { setName = "Diff, FieldMismatch, level 1"
      , leftValue = value1
      , rightValue = value2
      , expectedDiffResult = Error error2
      }
  , TestSet
      { setName = "Diff, FieldMismatch, level 2, WrongLengths"
      , leftValue = value1
      , rightValue = value3
      , expectedDiffResult = Error error3
      }
  , TestSet
      { setName = "Diff, FieldMismatch, level 2, DiffAtIndex"
      , leftValue = value1
      , rightValue = value4
      , expectedDiffResult = Error error4
      }
  ]
  where
    value1 = Tree.Node 1 [Tree.Node 2 [], Tree.Node 3 [Tree.Node 4 [], Tree.Node 5 []]]

    value2 = Tree.Node 2 []
    error2 = DiffSpecial $ FieldMismatch $ DiffAtField $ Z $ nodeInfo :*: Z TopLevelNotEqual

    value3 = Tree.Node 1 [Tree.Node 2 []]
    error3 =
      let e = DiffSpecial $ WrongLengths 2 1
      in  DiffSpecial $ FieldMismatch $ DiffAtField $ Z $ nodeInfo :*: S (Z e)

    value4 = Tree.Node 1 [Tree.Node 2 [], Tree.Node 4 []]
    error4 =
      let e = DiffSpecial $ DiffAtIndex 1 $ DiffSpecial $ FieldMismatch $ DiffAtField $ Z $ nodeInfo :*: Z TopLevelNotEqual
      in  DiffSpecial $ FieldMismatch $ DiffAtField $ Z $ nodeInfo :*: S (Z e)

    nodeInfo :: ConstructorInfo '[Int, [Tree Int]]
    nodeInfo :* _ = constructorInfo $ gdatatypeInfo $ Proxy @(Tree Int)

customTreeTestSets :: [TestSet (CustomTree Int)]
customTreeTestSets =
  [ TestSet
      { setName = "Equal"
      , leftValue = value1
      , rightValue = value1
      , expectedDiffResult = Equal
      }
  , TestSet
      { setName = "Diff, DiffAtNode, level 1"
      , leftValue = value1
      , rightValue = value2
      , expectedDiffResult = Error error2
      }
  , TestSet
      { setName = "Diff, WrongLengthsOfChildren, level 2"
      , leftValue = value1
      , rightValue = value3
      , expectedDiffResult = Error error3
      }
  , TestSet
      { setName = "Diff, DiffAtNode, level 2"
      , leftValue = value1
      , rightValue = value4
      , expectedDiffResult = Error error4
      }
  ]
  where
    value1 = CustomTree $ Tree.Node 1 [Tree.Node 2 [], Tree.Node 3 [Tree.Node 4 [], Tree.Node 5 []]]

    value2 = CustomTree $ Tree.Node 2 []
    error2 = DiffSpecial $ DiffAtNode (TreePath []) TopLevelNotEqual

    value3 = CustomTree $ Tree.Node 1 [Tree.Node 2 []]
    error3 = DiffSpecial $ WrongLengthsOfChildren (TreePath []) 2 1

    value4 = CustomTree $ Tree.Node 1 [Tree.Node 2 [], Tree.Node 4 []]
    error4 = DiffSpecial $ DiffAtNode (TreePath [1]) TopLevelNotEqual
