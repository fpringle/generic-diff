{-# LANGUAGE EmptyCase #-}

module Generics.Diff.Type where

import Data.List.NonEmpty (NonEmpty (..))
import Data.SOP.NP
import qualified Data.Text.Lazy.Builder as TB
import Generics.SOP as SOP
import Numeric.Natural

------------------------------------------------------------
-- Types

{- | A newtype wrapping a binary function producing a 'DiffResult'.
The only reason for this newtype is so that we can use it as a functor with the types from
@generic-sop@.
-}
newtype Differ x = Differ (x -> x -> DiffResult x)

{- | A GADT representing an error during the diff algorithm - i.e. this tells us where and how two values differ.

The 'DiffSpecial' constructors for instances of 'SpecialDiff' are so that we can treat these types uniquely.
See 'SpecialDiff'.
-}
data DiffError a where
  -- | All we can say is that the values being compared are not equal.
  TopLevelNotEqual :: DiffError a
  -- | We've identified a diff at a certain constructor or field
  Nested :: DiffErrorNested (Code a) -> DiffError a
  -- | Special case for special cases
  DiffSpecial :: (SpecialDiff a) => SpecialDiffError a -> DiffError a

{- | If we did a normal 'Generics.Diff.gdiff' on a linked list, we'd have to recurse through one "level" of
'Generics.Diff.Diff's for each element of the input lists. The output would be really hard to read or understand.
Therefore this type lets us treat lists as a special case, depending on how they differ.
-}
data ListDiffError a
  = -- | If we find a difference when comparing the two lists pointwise, we report the index of the
    -- error and the error caused by the elements at that index of the input lists.
    DiffAtIndex Int (DiffError a)
  | -- | The two lists have different lengths. If we get a 'WrongLengths' instead of an 'Equal' or a
    -- 'DiffAtIndex' , we know that one of the lists must be a subset of the other.
    WrongLengths Int Int
  deriving (Show, Eq)

infixr 6 :*:

-- | Lifted product of functors. We could have used 'Data.Functor.Product.Product', but this is more concise.
data (f :*: g) a = f a :*: g a
  deriving (Show, Eq)

{- | This is where we actually detail the difference between two values, and where in their structure the
difference is.
-}
data DiffErrorNested xss
  = -- | The two input values use different constructor, which are included.
    WrongConstructor (NS ConstructorInfo xss) (NS ConstructorInfo xss)
  | -- | The inputs use the same constructor, but differ at one of the fields.
    -- 'DiffAtField' will tell us where and how.
    FieldMismatch (DiffAtField xss)

-- | The result of a 'Generics.Diff.diff'.
data DiffResult a
  = -- | There's a diff, here it is
    Error (DiffError a)
  | -- | No diff, inputs are equal
    Equal
  deriving (Show, Eq)

{- | In the case that two values have the same constructor but differ at a certain field, we want two
report two things: what the 'DiffError' is at that field, and exactly where that field is. Careful use
of 'NS' gives us both of those things.
-}
newtype DiffAtField xss = DiffAtField (NS (ConstructorInfo :*: NS DiffError) xss)

------------------------------------------------------------
-- Classes

{- | Sometimes we want to diff types that don't quite fit the structor of a 'DiffErrorNested',
such as lists (see 'ListDiffError'), or even user-defined types that internally preserve invariants
or have unusual 'Eq' instances. In this case we can implement an instance of 'SpecialDiff' for the
type.
-}
class (Show (SpecialDiffError a), Eq (SpecialDiffError a)) => SpecialDiff a where
  -- | A custom diff error type for the special case.
  type SpecialDiffError a

  -- | Compare two values. The result will be converted to a 'DiffResult': 'Nothing' will result
  -- in 'Equal', whereas a 'Just' result will be converted to a 'DiffError' using 'DiffSpecial'.
  specialDiff :: a -> a -> Maybe (SpecialDiffError a)

  -- | As well as specifying how two diff two values, we also have to specify how to render
  -- the output. See the helper functions in "Generics.Diff.Render".
  renderSpecialDiffError :: SpecialDiffError a -> Doc

------------------------------------------------------------
-- Rendering

{- | Configuration type used to tweak the output of 'Generics.Diff.Render.renderDiffResultWith'.

Use 'Generics.Diff.Render.defaultRenderOpts' and the field accessors below to construct.
-}
data RenderOpts = RenderOpts
  { indentSize :: Natural
  -- ^ How many spaces to indent each new "level" of comparison.
  , numberedLevels :: Bool
  -- ^ Whether or not to include level numbers in the output.
  }
  deriving (Show)

{- | An intermediate representation for diff output.

We constrain output to follow a very simple pattern:

- 'docLines' is a non-empty series of preliminary lines describing the error.
- 'docSubDoc' is an optional 'Doc' representing a nested error, e.g. in 'FieldMismatch'.
-}
data Doc = Doc
  { docLines :: NonEmpty TB.Builder
  , docSubDoc :: Maybe Doc
  }
  deriving (Show)

------------------------------------------------------------
-- Instance madness

deriving instance (Show (DiffError a))

deriving instance (Eq (DiffError a))

eqPair :: (f a -> f a -> Bool) -> (g a -> g a -> Bool) -> (f :*: g) a -> (f :*: g) a -> Bool
eqPair onF onG (f1 :*: g1) (f2 :*: g2) =
  onF f1 f2 && onG g1 g2

showsPair :: (Int -> f a -> ShowS) -> (Int -> g a -> ShowS) -> Int -> (f :*: g) a -> ShowS
showsPair onF onG d (fa :*: ga) =
  showParen (d > 5) $
    onF 6 fa
      . showString " :*: "
      . onG 5 ga

instance Eq (DiffAtField xss) where
  DiffAtField mss == DiffAtField nss =
    eqNS (eqPair eqConstructorInfo (eqNS (==))) mss nss

instance Show (DiffAtField xss) where
  showsPrec d (DiffAtField ns) =
    showParen (d > 10) $
      showString "DiffAtField "
        . showsNS (showsPair showsConstructorInfo (showsNS showsPrec)) 11 ns

eqNP :: forall f xs. (SListI xs) => (forall x. f x -> f x -> Bool) -> NP f xs -> NP f xs -> Bool
eqNP eq l r = and $ collapse_NP $ liftA2_NP (\x y -> K (eq x y)) l r

showsNP :: forall f xs. (forall x. Int -> f x -> ShowS) -> Int -> NP f xs -> ShowS
showsNP _ _ Nil = showString "Nil"
showsNP s d ns = go d ns
  where
    go :: forall ys. Int -> NP f ys -> ShowS
    go p = \case
      Nil -> showString "Nil"
      x :* xs -> showParen (p > 5) $ s 6 x . showString " :* " . go 5 xs

eqNS :: forall f xs. (forall x. f x -> f x -> Bool) -> NS f xs -> NS f xs -> Bool
eqNS eq = compare_NS False eq False

showsNS :: forall f xs. (forall x. Int -> f x -> ShowS) -> Int -> NS f xs -> ShowS
showsNS s = go
  where
    go :: forall ys. Int -> NS f ys -> ShowS
    go d =
      showParen (d > 10) . \case
        Z z -> showString "Z " . s 11 z
        S zs -> showString "S " . go 11 zs

eqFieldInfo :: FieldInfo x -> FieldInfo x -> Bool
eqFieldInfo (FieldInfo fn1) (FieldInfo fn2) = fn1 == fn2

eqConstructorInfo :: ConstructorInfo xs -> ConstructorInfo xs -> Bool
eqConstructorInfo (Constructor cn1) (Constructor cn2) = cn1 == cn2
eqConstructorInfo (Infix cn1 a1 f1) (Infix cn2 a2 f2) =
  cn1 == cn2 && a1 == a2 && f1 == f2
eqConstructorInfo (Record cn1 f1) (Record cn2 f2) =
  cn1 == cn2 && eqNP eqFieldInfo f1 f2
eqConstructorInfo _ _ = False

showsConstructorInfo :: Int -> ConstructorInfo xs -> ShowS
showsConstructorInfo d =
  showParen (d > 10) . \case
    Constructor name -> showString "Constructor " . showsPrec 11 name
    Infix name ass fix ->
      showString "Infix "
        . showsPrec 11 name
        . showString " "
        . showsPrec 11 ass
        . showString " "
        . showsPrec 11 fix
    Record name fields ->
      showString "Record "
        . showsPrec 11 name
        . showString " "
        . showsNP showsPrec 11 fields

instance Eq (DiffErrorNested xss) where
  WrongConstructor l1 r1 == WrongConstructor l2 r2 =
    eqNS eqConstructorInfo l1 l2 && eqNS eqConstructorInfo r1 r2
  FieldMismatch al1 == FieldMismatch al2 = al1 == al2
  _ == _ = False

instance Show (DiffErrorNested xss) where
  showsPrec d = \case
    WrongConstructor l r ->
      showParen (d > 10) $
        showString "WrongConstructor "
          . showsNS showsConstructorInfo 11 l
          . showString " "
          . showsNS showsConstructorInfo 11 r
    FieldMismatch al ->
      showParen (d > 10) $
        showString "FieldMismatch "
          . showsPrec 11 al

pickOut :: NP f xs -> NS g xs -> NS f xs
pickOut Nil gs = case gs of {}
pickOut (x :* _) (Z _) = Z x
pickOut (_ :* xs) (S gs) = S $ pickOut xs gs
