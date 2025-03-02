{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-partial-fields -Wno-redundant-constraints #-}

module Diff where

import Data.SOP
import Data.SOP.NP
import qualified GHC.Generics as G
import Generics.SOP

newtype DiffError a = DiffError (DiffErrorNested (Code a))
  deriving (Show, Eq)

infixr 6 :*:

data (f :*: g) a = f a :*: g a
  deriving (Show, Eq)

eqPair :: (f a -> f a -> Bool) -> (g a -> g a -> Bool) -> (f :*: g) a -> (f :*: g) a -> Bool
eqPair onF onG (f1 :*: g1) (f2 :*: g2) =
  onF f1 f2 && onG g1 g2

showsPair :: (Int -> f a -> ShowS) -> (Int -> g a -> ShowS) -> Int -> (f :*: g) a -> ShowS
showsPair onF onG d (fa :*: ga) =
  showParen (d > 5) $
    onF 6 fa
      . showString " :*: "
      . onG 5 ga

newtype AtLoc f xss = AtLoc (NS (ConstructorInfo :*: NS f) xss)

eqAtLoc :: (forall x. f x -> f x -> Bool) -> AtLoc f xss -> AtLoc f xss -> Bool
eqAtLoc f (AtLoc mss) (AtLoc nss) =
  eqNS (eqPair eqConstructorInfo (eqNS f)) mss nss

showsAtLoc ::
  (forall x. Int -> f x -> ShowS) ->
  Int ->
  AtLoc f xss ->
  ShowS
showsAtLoc f d (AtLoc ns) =
  showParen (d > 10) $
    showString "AtLoc "
      . showsNS (showsPair showsConstructorInfo (showsNS f)) 11 ns

data DiffErrorNested xss
  = TopLevelNotEqual
  | WrongConstructor (NS ConstructorInfo xss) (NS ConstructorInfo xss)
  | FieldMismatch (AtLoc DiffError xss)

shiftAtLoc :: AtLoc f xs -> AtLoc f (x ': xs)
shiftAtLoc (AtLoc ns) = AtLoc (S ns)

shiftDiffError :: DiffErrorNested xs -> DiffErrorNested (x ': xs)
shiftDiffError = \case
  TopLevelNotEqual -> TopLevelNotEqual
  WrongConstructor xs ys -> WrongConstructor (S xs) (S ys)
  FieldMismatch atLoc -> FieldMismatch (shiftAtLoc atLoc)

data DiffResult a
  = Error (DiffError a)
  | Equal
  deriving (Show, Eq)

newtype Differ x = Differ (x -> x -> DiffResult x)

class Diff a where
  diff :: a -> a -> DiffResult a

diffEq :: (Eq a) => a -> a -> DiffResult a
diffEq a b =
  if a == b
    then Equal
    else Error $ DiffError TopLevelNotEqual

#define DIFF_EQ(a) instance Diff a where diff = diffEq
#define CDIFF_EQ(c,a) instance c => Diff a where diff = diffEq
#define GDIFF(a) instance Diff a where diff = gdiff
#define CGDIFF(c,a) instance c => Diff a where diff = gdiff
DIFF_EQ (Int)
DIFF_EQ (Char)
DIFF_EQ (Bool)
DIFF_EQ (String)
CGDIFF ((Diff a, Diff b), (Either a b))
CGDIFF (Diff a, (Maybe a))

gdiff ::
  forall a.
  (Generic a, HasDatatypeInfo a, All2 Diff (Code a)) =>
  a ->
  a ->
  DiffResult a
gdiff = gdiffWith $ cpure_POP (Proxy @Diff) (Differ diff)

gdiffWith ::
  forall a.
  (Generic a, HasDatatypeInfo a) =>
  POP Differ (Code a) ->
  a ->
  a ->
  DiffResult a
gdiffWith (POP ds) (from -> SOP xs) (from -> SOP ys) =
  maybe Equal (Error . DiffError) $ gdiff' (constructorInfo $ datatypeInfo $ Proxy @a) ds xs ys

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

pickOut :: NP f xs -> NS g xs -> NS f xs
pickOut Nil gs = case gs of {}
pickOut (x :* _) (Z _) = Z x
pickOut (_ :* xs) (S gs) = S $ pickOut xs gs

------------------------------------------------------------
-- Instance madness

eqNP :: forall f xs. (forall x. f x -> f x -> Bool) -> NP f xs -> NP f xs -> Bool
eqNP eq = go
  where
    go :: forall ys. NP f ys -> NP f ys -> Bool
    go Nil Nil = True
    go (x :* xs) (y :* ys) = eq x y && go xs ys

eqNS :: forall f xs. (forall x. f x -> f x -> Bool) -> NS f xs -> NS f xs -> Bool
eqNS eq = go
  where
    go :: forall ys. NS f ys -> NS f ys -> Bool
    go (Z x) (Z y) = eq x y
    go (Z _) (S _) = False
    go (S _) (Z _) = False
    go (S xs) (S ys) = go xs ys

eqFieldInfo :: FieldInfo x -> FieldInfo x -> Bool
eqFieldInfo (FieldInfo fn1) (FieldInfo fn2) = fn1 == fn2

eqConstructorInfo :: ConstructorInfo xs -> ConstructorInfo xs -> Bool
eqConstructorInfo (Constructor cn1) (Constructor cn2) = cn1 == cn2
eqConstructorInfo (Infix cn1 a1 f1) (Infix cn2 a2 f2) =
  cn1 == cn2 && a1 == a2 && f1 == f2
eqConstructorInfo (Record cn1 f1) (Record cn2 f2) =
  cn1 == cn2 && eqNP eqFieldInfo f1 f2
eqConstructorInfo _ _ = False

instance Eq (DiffErrorNested xss) where
  TopLevelNotEqual == TopLevelNotEqual = True
  WrongConstructor l1 r1 == WrongConstructor l2 r2 =
    eqNS eqConstructorInfo l1 l2 && eqNS eqConstructorInfo r1 r2
  FieldMismatch al1 == FieldMismatch al2 = eqAtLoc (==) al1 al2
  _ == _ = False

showsNS :: forall f xs. (forall x. Int -> f x -> ShowS) -> Int -> NS f xs -> ShowS
showsNS s = go
  where
    go :: forall ys. Int -> NS f ys -> ShowS
    go d =
      showParen (d > 10) . \case
        Z z -> showString "Z " . s 11 z
        S zs -> showString "S " . go 11 zs

showsNP :: forall f xs. (forall x. Int -> f x -> ShowS) -> Int -> NP f xs -> ShowS
showsNP _ _ Nil = showString "Nil"
showsNP s d ns = go d ns
  where
    go :: forall ys. Int -> NP f ys -> ShowS
    go p = \case
      Nil -> showString "Nil"
      x :* xs -> showParen (p > 5) $ s 6 x . showString " :* " . go 5 xs

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

collapseNS :: forall f r xs. (forall a. f a -> r) -> NS f xs -> r
collapseNS f = go
  where
    go :: forall ys. NS f ys -> r
    go = \case
      Z x -> f x
      S xs -> go xs

instance Show (DiffErrorNested xss) where
  showsPrec d = \case
    TopLevelNotEqual -> showString "TopLevelNotEqual"
    WrongConstructor l r ->
      showParen (d > 10) $
        showString "WrongConstructor "
          . showsNS showsConstructorInfo 11 l
          . showString " "
          . showsNS showsConstructorInfo 11 r
    FieldMismatch al ->
      showParen (d > 10) $
        showString "FieldMismatch "
          -- . showsNS (showsNS showsPrec) 11 nss
          . showsAtLoc showsPrec 11 al

type AllCompose c f xss = All (Compose c f) xss

------------------------------------------------------------
-- test type

data Test
  = C1 (Either Int String) (Maybe Char)
  | C2 {c11 :: Maybe String, c12 :: Bool}
  deriving (Show, Eq, G.Generic)

instance Generic Test

instance HasDatatypeInfo Test

c1Info :: ConstructorInfo [Either Int String, Maybe Char]
c2Info :: ConstructorInfo [Maybe String, Bool]
c1Info :* c2Info :* _ = constructorInfo $ datatypeInfo $ Proxy @Test

drExps :: [(Test, Test, DiffResult Test)]
drExps =
  [
    ( C1 (Left 0) Nothing
    , C1 (Left 0) Nothing
    , Equal
    )
  ,
    ( C1 (Left 0) Nothing
    , C1 (Left 0) (Just 'a')
    , Error (DiffError $ FieldMismatch (AtLoc (Z (Constructor "C1" :*: S (Z $ DiffError TopLevelNotEqual)))))
    )
  ,
    ( C1 (Left 0) Nothing
    , C2 Nothing False
    , Error (DiffError $ WrongConstructor (Z c1Info) (S (Z c2Info)))
    )
  ]

drActs :: [(Test, Test, DiffResult Test)]
drActs = [(t1, t2, gdiff t1 t2) | (t1, t2, _) <- drExps]
