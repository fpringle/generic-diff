{-# OPTIONS_GHC -Wno-partial-fields #-}

module Generics.Diff.PropertyTestsSpec where

import Control.Monad
import Data.Complex
import Data.Fixed
import qualified Data.Functor.Compose as F
import Data.Functor.Const
import Data.Functor.Identity
import Data.Functor.Product
import qualified Data.Monoid as M
import Data.Proxy
import qualified Data.Semigroup as S
import Data.Version
import Foreign.C.Types
import Generics.Diff
import Generics.Diff.Instances ()
import Generics.Diff.UnitTestsSpec
import qualified Test.Hspec as H
import qualified Test.Hspec.QuickCheck as H
import qualified Test.QuickCheck as Q
import Util

spec :: H.Spec
spec = do
  H.describe "x == y => x `diff` y == Equal" $
    manyTypes True propEqualGivesEqual
  H.describe "x `diff` y == Equal => x == y" $
    manyTypes False propEqualMeansEqual

-- | If the two inputs are equal, 'diff' should return 'Equal'.
propEqualGivesEqual :: forall a. (Q.Arbitrary a, Diff a, Show a) => Proxy a -> Q.Property
propEqualGivesEqual _ = Q.property $ \a -> propDiffResult @a a a Equal

-- | If the two inputs are not equal, 'diff' should never return 'Equal'.
propEqualMeansEqual :: forall a. (Q.Arbitrary a, Eq a, Diff a, Show a) => Proxy a -> Q.Property
propEqualMeansEqual _ = Q.property $ \leftValue rightValue ->
  leftValue /= rightValue Q.==>
    diff @a leftValue rightValue /= Equal

manyTypes :: Bool -> (forall x. (Q.Arbitrary x, Eq x, Diff x, Show x) => Proxy x -> Q.Property) -> H.Spec
manyTypes allowUnit prop = do
  H.describe "Primitive/opaque types" $ do
    when allowUnit $ H.prop "()" $ prop $ Proxy @()
    H.prop "Bool" $ prop $ Proxy @Char
    H.prop "Char" $ prop $ Proxy @Char
    H.prop "Int" $ prop $ Proxy @Int
    H.prop "Rational" $ prop $ Proxy @Rational
    H.prop "Version" $ prop $ Proxy @Version
    H.prop "CLong" $ prop $ Proxy @CLong
    H.prop "CChar" $ prop $ Proxy @CChar
    H.prop "Uni" $ prop $ Proxy @Uni
    H.prop "Deci" $ prop $ Proxy @Deci
    H.prop "Centi" $ prop $ Proxy @Centi
  H.describe "Newtypes" $ do
    when allowUnit $ H.prop "Identity ()" $ prop $ Proxy @(Identity ())
    H.prop "Identity Char" $ prop $ Proxy @(Identity Char)
    H.prop "Identity Int" $ prop $ Proxy @(Identity Int)

    H.prop "Dual Rational" $ prop $ Proxy @(M.Dual Rational)
    H.prop "Dual Version" $ prop $ Proxy @(M.Dual Version)
    H.prop "Dual CLong" $ prop $ Proxy @(M.Dual CLong)

    H.prop "Sum CChar" $ prop $ Proxy @(M.Sum CChar)
    H.prop "Sum Uni" $ prop $ Proxy @(M.Sum Uni)
    H.prop "Sum Deci" $ prop $ Proxy @(M.Sum Deci)

    H.prop "First ()" $ prop $ Proxy @(M.First ())
    H.prop "First Char" $ prop $ Proxy @(M.First Char)
    H.prop "First Int" $ prop $ Proxy @(M.First Int)

    H.prop "Alt Identity Rational" $ prop $ Proxy @(M.Alt Identity Rational)
    H.prop "Alt Identity Version" $ prop $ Proxy @(M.Alt Identity Version)
    H.prop "Alt Identity CLong" $ prop $ Proxy @(M.Alt Identity CLong)

    H.prop "Complex CChar" $ prop $ Proxy @(Complex CChar)
    H.prop "Complex Uni" $ prop $ Proxy @(Complex Uni)

    H.prop "Any" $ prop $ Proxy @S.Any
    H.prop "All" $ prop $ Proxy @S.All

    H.prop "Const Any Bool" $ prop $ Proxy @(Const S.Any Bool)
    H.prop "Const All ()" $ prop $ Proxy @(Const S.All ())

  H.describe "Combinator types" $ do
    H.prop "Either Int Char" $ prop $ Proxy @(Either Int Char)
    H.prop "Maybe String" $ prop $ Proxy @(Maybe String)
    H.prop "[Bool]" $ prop $ Proxy @[Bool]
    H.prop "Compose Maybe (Either Int) Char" $ prop $ Proxy @(F.Compose Maybe (Either Int) Char)
    H.prop "Product Maybe (Either Int) Char" $ prop $ Proxy @(Product Maybe (Either Int) Char)

  H.describe "Tuples" $ do
    H.prop "((), Char)" $ prop $ Proxy @((), Char)
    H.prop "(Int, Rational, Version)" $ prop $ Proxy @(Int, Rational, Version)
    H.prop "(CLong, CChar, Uni, Deci)" $ prop $ Proxy @(CLong, CChar, Uni, Deci)

  H.describe "Custom types" $ do
    H.prop "CustomType" $ prop $ Proxy @CustomType
