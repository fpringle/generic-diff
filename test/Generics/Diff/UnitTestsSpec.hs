{-# OPTIONS_GHC -Wno-partial-fields #-}

module Generics.Diff.UnitTestsSpec where

import Data.Foldable
import Data.SOP
import qualified Data.Text as T
import qualified GHC.Generics as G
import Generics.Diff
import Generics.Diff.Instances ()
import Generics.SOP
import qualified Test.Hspec as H
import qualified Test.Hspec.QuickCheck as H
import qualified Test.QuickCheck as Q

spec :: H.Spec
spec =
  H.describe "Unit tests using hand-rolled datatypes" $
    traverse_ specTestSet testSets

specTestSet :: (Diff a, Show a) => TestSet a -> H.Spec
specTestSet TestSet {..} =
  let actualDiffResult = diff leftValue rightValue
      eq = expectedDiffResult == actualDiffResult
      showDiffResult = show
      addLabel =
        if eq
          then Q.property
          else
            Q.counterexample ("Expected DiffResult:\n" <> showDiffResult expectedDiffResult)
              . Q.counterexample ("Actual DiffResult:\n" <> showDiffResult actualDiffResult)
              . Q.counterexample ("Left value:\n" <> show leftValue)
              . Q.counterexample ("Right value:\n" <> show rightValue)
  in  H.prop (T.unpack setName) $ addLabel eq

------------------------------------------------------------
-- test type

data CustomType
  = -- | Normal constructor
    Con1 (Either Int String) (Maybe Char)
  | -- | Record constructor
    Con2 {c11 :: Maybe String, c12 :: Bool}
  | -- | Infix constructor
    (Char, Int, ()) `Con3` [Maybe Int]
  | -- | Recursive
    Con4 CustomType CustomType
  deriving (Show, Eq, G.Generic)

instance Generic CustomType

instance HasDatatypeInfo CustomType

instance Diff CustomType

-- bunch of 'ConstructorInfo's, for convenience

c1Info :: ConstructorInfo '[Either Int String, Maybe Char]
c2Info :: ConstructorInfo '[Maybe String, Bool]
c3Info :: ConstructorInfo '[(Char, Int, ()), [Maybe Int]]
c4Info :: ConstructorInfo '[CustomType, CustomType]
c1Info :* c2Info :* c3Info :* c4Info :* _ = constructorInfo $ datatypeInfo $ Proxy @CustomType

nothingInfo :: ConstructorInfo '[]
justInfo :: forall a. ConstructorInfo '[a]
nothingInfo :* justInfo :* _ = constructorInfo $ datatypeInfo (Proxy :: Proxy (Maybe a))

data TestSet a = TestSet
  { setName :: T.Text
  , leftValue :: a
  , rightValue :: a
  , expectedDiffResult :: DiffResult a
  }
  deriving (Show)

-- | some pairs of 'CustomType's, with the 'DiffResult' we expect to get for each of them using 'gdiff'.
testSets :: [TestSet CustomType]
testSets =
  [ TestSet
      { setName = "Equal - Con1"
      , leftValue = value1
      , rightValue = value1
      , expectedDiffResult = Equal
      }
  , TestSet
      { setName = "Diff, WrongConstructor, Con3 and Con1"
      , leftValue = leftValue2
      , rightValue = rightValue2
      , expectedDiffResult = Error error2
      }
  , TestSet
      { setName = "Diff, WrongConstructor, Con3 and Con1"
      , leftValue = ('a', 5, ()) `Con3` [Just 1]
      , rightValue = Con1 (Left 0) (Just 'a')
      , expectedDiffResult = Error (Nested $ WrongConstructor (S (S (Z c3Info))) (Z c1Info))
      }
  , TestSet
      { setName = "Diff, FieldMismatch, normal Constructor, nested"
      , leftValue = Con1 (Left 0) Nothing
      , rightValue = Con1 (Left 0) (Just 'a')
      , expectedDiffResult = Error (Nested $ FieldMismatch (DiffAtField (Z (c1Info :*: S (Z $ Nested (WrongConstructor (Z nothingInfo) (S (Z justInfo))))))))
      }
  , TestSet
      { setName = "Diff, FieldMismatch, Infix constructor, right side, nested"
      , leftValue = ('a', 5, ()) `Con3` [Just 1]
      , rightValue = ('a', 5, ()) `Con3` [Nothing, Just 1]
      , expectedDiffResult = Error (Nested $ FieldMismatch (DiffAtField (S (S (Z (c3Info :*: S (Z $ DiffList (DiffAtIndex 0 (Nested (WrongConstructor (S (Z justInfo)) (Z nothingInfo)))))))))))
      }
  , TestSet
      { setName = "Diff, FieldMismatch, Infix constructor, left side, nested"
      , leftValue = ('a', 4, ()) `Con3` [Just 1]
      , rightValue = ('a', 5, ()) `Con3` [Nothing, Just 1]
      , expectedDiffResult = Error (Nested $ FieldMismatch (DiffAtField (S (S (Z (c3Info :*: Z (Nested $ FieldMismatch $ DiffAtField $ Z (Constructor "(,,)" :*: S (Z TopLevelNotEqual)))))))))
      }
  , TestSet
      { setName = "Diff, FieldMismatch, recursive"
      , leftValue = Con4 value1 leftValue2
      , rightValue = Con4 value1 rightValue2
      , expectedDiffResult = Error (Nested $ FieldMismatch (DiffAtField (S (S (S (Z (c4Info :*: S (Z error2))))))))
      }
  ]
  where
    value1 = Con1 (Left 0) Nothing

    leftValue2 = Con1 (Left 0) Nothing
    rightValue2 = Con2 Nothing False
    error2 = Nested $ WrongConstructor (Z c1Info) (S (Z c2Info))

-- | The expected 'DiffResult's of 'testSets'
expectedGDiffResults :: [DiffResult CustomType]
expectedGDiffResults = expectedDiffResult <$> testSets

-- | The actual 'DiffResult's of 'testSets' - from calling 'gdiff'.
actualGDiffResults :: [DiffResult CustomType]
actualGDiffResults = [gdiff leftValue rightValue | TestSet {..} <- testSets]
