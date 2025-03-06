{-# LANGUAGE EmptyCase #-}

module Generics.Diff.Type where

import Data.SOP.NP
import Generics.SOP as SOP

data DiffError a where
  Nested :: DiffErrorNested (Code a) -> DiffError a
  DiffList :: ListDiffError a -> DiffError [a]

data ListDiffError a
  = DiffAtIndex Int (DiffError a)
  | WrongLengths Int Int
  deriving (Show, Eq)

deriving instance (Show (DiffError a))

deriving instance (Eq (DiffError a))

infixr 6 :*:

data (f :*: g) a = f a :*: g a
  deriving (Show, Eq)

data DiffErrorNested xss
  = TopLevelNotEqual
  | WrongConstructor (NS ConstructorInfo xss) (NS ConstructorInfo xss)
  | FieldMismatch (AtLoc DiffError xss)

data DiffResult a
  = Error (DiffError a)
  | Equal
  deriving (Show, Eq)

newtype AtLoc f xss = AtLoc (NS (ConstructorInfo :*: NS f) xss)

------------------------------------------------------------
-- Instance madness

eqPair :: (f a -> f a -> Bool) -> (g a -> g a -> Bool) -> (f :*: g) a -> (f :*: g) a -> Bool
eqPair onF onG (f1 :*: g1) (f2 :*: g2) =
  onF f1 f2 && onG g1 g2

showsPair :: (Int -> f a -> ShowS) -> (Int -> g a -> ShowS) -> Int -> (f :*: g) a -> ShowS
showsPair onF onG d (fa :*: ga) =
  showParen (d > 5) $
    onF 6 fa
      . showString " :*: "
      . onG 5 ga

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
  TopLevelNotEqual == TopLevelNotEqual = True
  WrongConstructor l1 r1 == WrongConstructor l2 r2 =
    eqNS eqConstructorInfo l1 l2 && eqNS eqConstructorInfo r1 r2
  FieldMismatch al1 == FieldMismatch al2 = eqAtLoc (==) al1 al2
  _ == _ = False

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
          . showsAtLoc showsPrec 11 al

pickOut :: NP f xs -> NS g xs -> NS f xs
pickOut Nil gs = case gs of {}
pickOut (x :* _) (Z _) = Z x
pickOut (_ :* xs) (S gs) = S $ pickOut xs gs
