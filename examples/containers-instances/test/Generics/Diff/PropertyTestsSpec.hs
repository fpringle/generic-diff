{-# OPTIONS_GHC -Wno-partial-fields #-}

module Generics.Diff.PropertyTestsSpec where

import Data.Fixed
import Data.Map (Map)
import Data.Proxy
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Tree (Tree)
import Data.Version
import Foreign.C.Types
import Generics.Diff
import Generics.Diff.Instances ()
import Generics.Diff.Special.Map ()
import Generics.Diff.Special.Seq ()
import Generics.Diff.Special.Set ()
import Generics.Diff.Special.Tree ()
import qualified Test.Hspec as H
import qualified Test.Hspec.QuickCheck as H
import qualified Test.QuickCheck as Q
import Util

spec :: H.Spec
spec = do
  H.describe "x == y => x `diff` y == Equal" $
    manyTypes propEqualGivesEqual
  H.describe "x `diff` y == Equal => x == y" $
    manyTypes propEqualMeansEqual

-- | If the two inputs are equal, 'diff' should return 'Equal'.
propEqualGivesEqual :: forall a. (Q.Arbitrary a, Diff a, Show a) => Proxy a -> Q.Property
propEqualGivesEqual _ = Q.property $ \a -> propDiffResult @a a a Equal

-- | If the two inputs are not equal, 'diff' should never return 'Equal'.
propEqualMeansEqual :: forall a. (Q.Arbitrary a, Eq a, Diff a, Show a) => Proxy a -> Q.Property
propEqualMeansEqual _ = Q.property $ \leftValue rightValue ->
  leftValue /= rightValue Q.==>
    diff @a leftValue rightValue /= Equal

manyTypes :: (forall x. (Q.Arbitrary x, Eq x, Diff x, Show x) => Proxy x -> Q.Property) -> H.Spec
manyTypes prop = do
  H.prop "Set Char" $ prop $ Proxy @(Set Char)
  H.prop "Set Int" $ prop $ Proxy @(Set Int)

  H.prop "Seq Rational" $ prop $ Proxy @(Seq Rational)
  H.prop "Seq Version" $ prop $ Proxy @(Seq Version)
  H.prop "Seq CLong" $ prop $ Proxy @(Seq CLong)

  H.prop "Tree CChar" $ prop $ Proxy @(Tree CChar)
  H.prop "Tree Uni" $ prop $ Proxy @(Tree Uni)
  H.prop "Tree Deci" $ prop $ Proxy @(Tree Deci)

  H.prop "Map Int Char" $ prop $ Proxy @(Map Int Char)
  H.prop "Map Char Int" $ prop $ Proxy @(Map Char Int)
