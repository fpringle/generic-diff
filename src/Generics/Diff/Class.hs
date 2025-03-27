{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-orphans #-}

module Generics.Diff.Class
  ( -- * Class
    Diff (..)

    -- ** Implementing diff
  , gdiff
  , gdiffTopLevel
  , gdiffWith
  , eqDiff
  , diffWithSpecial
  , gspecialDiffNested

    -- * Special case: lists
  , diffListWith
  )
where

import Data.SOP
import Data.SOP.NP
import qualified GHC.Generics as G
import Generics.Diff.Render
import Generics.Diff.Type
import Generics.SOP as SOP
import Generics.SOP.GGP as SOP

{- | A type with an instance of 'Diff' permits a more nuanced comparison than 'Eq' or 'Ord'.
If two values are not equal, 'diff' will tell you exactly where they differ ("in this contructor,
at that field"). The granularity of the pinpointing of the difference (how many "levels" of 'Diff'
we can "descend" through) depends on the implementation of the instance.

For user-defined types, it's strongly recommended you derive your 'Diff' instance using 'Generic' from
@generics-sop@. If those types refer to other types, those will need 'Diff' instances too. For example:

@
{\-# LANGUAGE DerivingStrategies #-\}
{\-# LANGUAGE DeriveGeneric #-\}
{\-# LANGUAGE DeriveAnyClass #-\}

import qualified GHC.Generics as G
import qualified Generics.SOP as SOP

data BinOp = Plus | Minus
  deriving stock (Show, G.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, Diff)

data Expr
  = Atom Int
  | Bin {left :: Expr, op :: BinOp, right :: Expr}
  deriving stock (Show, G.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, Diff)
@

Now that we have our instances, we can 'diff' values to find out exactly where they differ:

@
-- If two values are equal, 'diff' should always return 'Equal'.
ghci> diff Plus Plus
Equal

ghci> diff Plus Minus
Error (Nested (WrongConstructor (Z (Constructor \"Plus\")) (S (Z (Constructor \"Minus\")))))

ghci> diff (Atom 1) (Atom 2)
Error (Nested (FieldMismatch (AtLoc (Z (Constructor \"Atom\" :*: Z (Nested TopLevelNotEqual))))))

ghci> diff (Bin (Atom 1) Plus (Atom 1)) (Atom 2)
Error (Nested (WrongConstructor (S (Z (Constructor \"Bin\"))) (Z (Constructor \"Atom\"))))

ghci> diff (Bin (Atom 1) Plus (Atom 1)) (Bin (Atom 1) Minus (Atom 1))
Error (Nested (FieldMismatch (AtLoc (S (Z (Constructor \"Bin\" :*: S (Z (Nested (WrongConstructor (Z (Constructor \"Plus\")) (S (Z (Constructor \"Minus\"))))))))))))

ghci> diff (Bin (Atom 1) Plus (Atom 1)) (Bin (Atom 1) Plus (Atom 2))
Error (Nested (FieldMismatch (DiffAtField (S (Z (Record \"Bin\" (FieldInfo \"left\" :* FieldInfo \"op\" :* FieldInfo \"right\" :* Nil) :*: S (S (Z (Nested (FieldMismatch (DiffAtField (Z (Constructor \"Atom\" :*: Z TopLevelNotEqual)))))))))))))
@

Of course, these are just as difficult to understand as derived 'Show' instances, or more so. Fortunately we can
use the functions in "Generics.Diff.Render" to get a nice, intuitive representation of the diffs:

@
ghci> printDiffResult $ diff Plus Plus
Equal

ghci> printDiffResult $ diff Plus Minus
Wrong constructor
Constructor of left value: Plus
Constructor of right value: Minus

ghci> printDiffResult $ diff (Atom 1) (Atom 2)
Both values use constructor Atom but fields don't match
In field 0 (0-indexed):
  Not equal

ghci> printDiffResult $ diff (Bin (Atom 1) Plus (Atom 1)) (Atom 2)
Wrong constructor
Constructor of left value: Bin
Constructor of right value: Atom

ghci> printDiffResult $ diff (Bin (Atom 1) Plus (Atom 1)) (Bin (Atom 1) Minus (Atom 1))
Both values use constructor Bin but fields don't match
In field op:
  Wrong constructor
  Constructor of left value: Plus
  Constructor of right value: Minus

ghci> printDiffResult $ diff (Bin (Atom 1) Plus (Atom 1)) (Bin (Atom 1) Plus (Atom 2))
Both values use constructor Bin but fields don't match
In field right:
  Both values use constructor Atom but fields don't match
  In field 0 (0-indexed):
    Not equal
@

= Laws

For type @a@ with @instance Diff a@, and values @x, y :: a@, the following should hold:

@
x == y   \<=\>  x \`diff\` y == 'Equal'
@
-}
class Diff a where
  -- | Detailed comparison of two values. It is strongly recommended to only use the
  -- default implementation, or one of 'eqDiff' or 'gdiffTopLevel'.
  diff :: a -> a -> DiffResult a
  default diff :: (Generic a, HasDatatypeInfo a, All2 Diff (Code a)) => a -> a -> DiffResult a
  diff = gdiff

  -- | Compare two lists of values. This mostly exists so that we can define a custom instance for 'String',
  -- in a similar vein to 'showList'.
  diffList :: [a] -> [a] -> DiffResult [a]
  diffList = diffWithSpecial

-- | When we have an instance of 'SpecialDiff', we can implement 'diff' using 'DiffSpecial'.
diffWithSpecial :: (SpecialDiff a) => a -> a -> DiffResult a
diffWithSpecial l r = maybe Equal (Error . DiffSpecial) $ specialDiff l r

instance (Diff a) => SpecialDiff [a] where
  type SpecialDiffError [a] = ListDiffError a
  specialDiff = diffListWith diff
  renderSpecialDiffError = listDiffErrorDoc "list"

{- | Given two lists and a way to 'diff' the elements of the list,
return a 'ListDiffError'. Used to implement 'specialDiff' for list-like types.
See "Generics.Diff.Special" for an example.
-}
diffListWith :: (a -> a -> DiffResult a) -> [a] -> [a] -> Maybe (ListDiffError a)
diffListWith d = go 0
  where
    go _ [] [] = Nothing
    go n [] ys = Just $ WrongLengths n (n + length ys)
    go n xs [] = Just $ WrongLengths (n + length xs) n
    go n (x : xs) (y : ys) = case d x y of
      Equal -> go (n + 1) xs ys
      Error err -> Just $ DiffAtIndex n err

{- | The most basic 'Differ' possible. If the two values are equal, return 'Equal';
otherwise, return 'TopLevelNotEqual'.
-}
eqDiff :: (Eq a) => a -> a -> DiffResult a
eqDiff a b =
  if a == b
    then Equal
    else Error TopLevelNotEqual

{- | The default implementation of 'diff'. Follows the procedure described above. We keep recursing
into the 'Diff' instances of the field types, as far as we can.
-}
gdiff ::
  forall a.
  (Generic a, HasDatatypeInfo a, All2 Diff (Code a)) =>
  a ->
  a ->
  DiffResult a
gdiff = gdiffWithPure @a @Diff (Differ diff)

{- | Alternate implementation of 'diff' - basically one level of 'gdiff'. To compare individual fields of the
top-level values, we just use '(==)'.
-}
gdiffTopLevel ::
  forall a.
  (Generic a, HasDatatypeInfo a, All2 Eq (Code a)) =>
  a ->
  a ->
  DiffResult a
gdiffTopLevel = gdiffWithPure @a @Eq (Differ eqDiff)

{- | Follow the same algorithm as 'gdiff', but the caller can provide their own 'POP' grid of 'Differ's
specifying how to compare each field we might come across.
-}
gdiffWith ::
  forall a.
  (Generic a, HasDatatypeInfo a) =>
  POP Differ (Code a) ->
  a ->
  a ->
  DiffResult a
gdiffWith (POP ds) (from -> SOP xs) (from -> SOP ys) =
  maybe Equal (Error . Nested) $ gdiff' (constructorInfo $ datatypeInfo $ Proxy @a) ds xs ys

gdiffWithPure ::
  forall a c.
  (Generic a, HasDatatypeInfo a, All2 c (Code a)) =>
  (forall x. (c x) => Differ x) ->
  a ->
  a ->
  DiffResult a
gdiffWithPure ds = gdiffWith $ cpure_POP (Proxy @c) ds

{- | Helper function to implement 'specialDiff' for an instance of "GHC.Generic", with
@SpecialDiffError a = DiffErrorNested xss@.

For example, say we want to implement 'SpecialDiff' (and then 'Diff') for @Tree@ from @containers@.
We'd ideally like to use a 'SOP.Generic' instance, but we don't have one. Nevertheless we can fake one,
using 'G.Generic' from "GHC.Generics".

@
data Tree a = Node
  { rootLabel :: a
  , subForest :: [Tree a]
  }
  deriving ('G.Generic')

instance ('Diff' a) => 'SpecialDiff' (Tree a) where
  type 'SpecialDiffError' (Tree a) = 'DiffErrorNested' ('GCode' (Tree a))
  'specialDiff' = 'gspecialDiffNested'

  'renderSpecialDiffError' = 'diffErrorNestedDoc'

instance ('Diff' a) => 'Diff' (Tree a) where
  diff = 'diffWithSpecial'
@
-}
gspecialDiffNested ::
  forall a.
  ( G.Generic a
  , GFrom a
  , GDatatypeInfo a
  , All2 Diff (GCode a)
  ) =>
  a ->
  a ->
  Maybe (DiffErrorNested (GCode a))
gspecialDiffNested l r = gdiff' constructors differs (unSOP $ gfrom l) (unSOP $ gfrom r)
  where
    differs = unPOP $ hcpure (Proxy @Diff) (Differ diff)
    constructors = constructorInfo $ gdatatypeInfo $ Proxy @a

------------------------------------------------------------
-- Auxiliary functions

{- | This is the workhorse of 'gdiff', 'gdiffWith' and 'gdiffTopLevel'. A 'Nothing' return value means there
were no errors (diffs), and so we can return 'Equal'. A 'Just' value means the two top-level values are
not equal, and tells us where.
-}
gdiff' ::
  NP ConstructorInfo xss ->
  NP (NP Differ) xss ->
  NS (NP I) xss ->
  NS (NP I) xss ->
  Maybe (DiffErrorNested xss)
gdiff' _ Nil xss _ = case xss of {}
gdiff' (i :* _) (ds :* _) (Z xs) (Z ys) =
  FieldMismatch . DiffAtField . Z . (i :*:) <$> goProduct ds xs ys
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

shiftDiffError :: DiffErrorNested xs -> DiffErrorNested (x ': xs)
shiftDiffError = \case
  WrongConstructor xs ys -> WrongConstructor (S xs) (S ys)
  FieldMismatch (DiffAtField ns) -> FieldMismatch (DiffAtField (S ns))
