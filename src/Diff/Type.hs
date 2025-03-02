{-# LANGUAGE CPP #-}

-- {-# OPTIONS_GHC -fdefer-typed-holes #-}

module Diff.Type where

import Data.Functor.Product
import Data.Kind
import Data.SOP.NP
import Generics.SOP as SOP
import Numeric.Natural

class Diff a where
  diff :: a -> a -> DiffResult

#define EQ_DIFF(x) instance Diff x where diff = eqDiff
#define TL_DIFF(c, x) instance c => Diff x where diff = diffRecursive

EQ_DIFF (Int)
TL_DIFF (Diff a, (Maybe a))
TL_DIFF ((Diff a, Diff b), (Either a b))

differ :: (Diff a) => Differ a
differ = Differ diff

diffTopLevel ::
  forall a.
  (HasDatatypeInfo a, All2 Eq (Code a)) =>
  a ->
  a ->
  DiffResult
diffTopLevel x y = gDiff differs info (from x) (from y)
  where
    info = datatypeInfo $ Proxy @a

    differs :: POP Differ (Code a)
    differs = hcpure (Proxy @Eq) (Differ eqDiff)

diffRecursive ::
  forall a.
  (HasDatatypeInfo a, All2 Diff (Code a)) =>
  a ->
  a ->
  DiffResult
diffRecursive x y = gDiff differs info (from x) (from y)
  where
    info = datatypeInfo $ Proxy @a

    differs :: POP Differ (Code a)
    differs = hcpure (Proxy @Diff) differ

data DiffStep
  = IntoConstructor ConstructorName
  | IntoNamedField FieldName
  | IntoUnnamedField Natural
  deriving (Show, Eq)

infixl 5 :>

data DiffPath
  = NilPath
  | DiffPath :> DiffStep
  deriving (Show, Eq)

appendPathStep :: DiffStep -> DiffPath -> DiffPath
appendPathStep step path = path :> step

data DiffError
  = WrongConstructor ConstructorName ConstructorName
  | NotEqual
  deriving (Show, Eq)

data DiffResult
  = Error DiffPath DiffError
  | Equal
  deriving (Show, Eq)

instance Semigroup DiffResult where
  Equal <> err = err
  err <> _ = err

instance Monoid DiffResult where
  mempty = Equal

overPath :: (DiffPath -> DiffPath) -> DiffResult -> DiffResult
overPath f = \case
  Error p err -> Error (f p) err
  Equal -> Equal

newtype Differ a = Differ (a -> a -> DiffResult)

eqDiff :: (Eq a) => a -> a -> DiffResult
eqDiff x y =
  if x == y
    then Equal
    else Error NilPath NotEqual

gDiff ::
  forall (xss :: [[Type]]).
  (All SListI xss) =>
  POP Differ xss ->
  DatatypeInfo xss ->
  SOP I xss ->
  SOP I xss ->
  DiffResult
gDiff dss info (SOP xss) (SOP yss) =
  case info of
    ADT _ _ consInfo _ ->
      let infoAndDiffers = hzipWith Pair (unPOP dss) consInfo

          onSameConstructor ::
            (SListI xs) =>
            Product (NP Differ) ConstructorInfo xs ->
            NP I xs ->
            NP I xs ->
            DiffResult
          onSameConstructor (Pair ds i) = gDiffProduct i ds

          onLeftFirst ::
            Product (NP Differ) ConstructorInfo x ->
            NP I x ->
            NS (NP Differ `Product` ConstructorInfo `Product` NP I) ys ->
            DiffResult
          onLeftFirst (Pair _ cInfo) _ r =
            let lCstr = constructorName cInfo
                rCstr = onZ (\(Pair (Pair _ cInfo') _) -> constructorName cInfo') r
            in  Error NilPath $ WrongConstructor lCstr rCstr

          onRightFirst ::
            Product (NP Differ) ConstructorInfo y ->
            NS (NP Differ `Product` ConstructorInfo `Product` NP I) xs ->
            NP I y ->
            DiffResult
          onRightFirst (Pair _ cInfo) l _ =
            let rCstr = constructorName cInfo
                lCstr = onZ (\(Pair (Pair _ cInfo') _) -> constructorName cInfo') l
            in  Error NilPath $ WrongConstructor lCstr rCstr
      in  compareNS' onLeftFirst onSameConstructor onRightFirst infoAndDiffers xss yss
    Newtype _ _ cInfo ->
      gDiffNewtype (unNP $ unNP $ unPOP dss) cInfo (unI $ unNP $ unZ xss) (unI $ unNP $ unZ yss)

onZ :: forall f r xs. (forall x. f x -> r) -> NS f xs -> r
onZ f = go
  where
    go :: forall ys. NS f ys -> r
    go = \case
      Z x -> f x
      S xs -> go xs

hzipWith4 ::
  forall f f' f'' f''' f'''' xs.
  (forall a. f a -> f' a -> f'' a -> f''' a -> f'''' a) ->
  NP f xs ->
  NP f' xs ->
  NP f'' xs ->
  NP f''' xs ->
  NP f'''' xs
hzipWith4 f = go
  where
    go :: forall ys. NP f ys -> NP f' ys -> NP f'' ys -> NP f''' ys -> NP f'''' ys
    go Nil Nil Nil Nil = Nil
    go (w :* ws) (x :* xs) (y :* ys) (z :* zs) =
      f w x y z :* go ws xs ys zs

gDiffProduct ::
  forall (xs :: [Type]).
  (SListI xs) =>
  ConstructorInfo xs ->
  NP Differ xs ->
  NP I xs ->
  NP I xs ->
  DiffResult
gDiffProduct = \case
  Constructor name -> gDiffProductConstructor name
  Infix name _ _ -> gDiffProductInfix name
  Record name fields -> gDiffProductRecord name fields

countNP' :: forall xs f. (SListI xs) => (forall a. Natural -> f a) -> NP f xs
countNP' f = go 0
  where
    go :: forall ys. (SListI ys) => Natural -> NP f ys
    go n = case sList @ys of
      SNil -> Nil
      SCons -> f n :* go (n + 1)

countNP :: forall xs. (SListI xs) => NP (K Natural) xs
countNP = countNP' K

gDiffProductConstructor ::
  forall xs.
  (SListI xs) =>
  ConstructorName ->
  NP Differ xs ->
  NP I xs ->
  NP I xs ->
  DiffResult
gDiffProductConstructor name ds xs ys = mconcat . collapse_NP $ hzipWith4 something ns ds xs ys
  where
    ns = countNP

    something :: forall a. K Natural a -> Differ a -> I a -> I a -> K DiffResult a
    something (K n) (Differ d) (I x) (I y) =
      let addInfo =
            overPath $
              appendPathStep (IntoUnnamedField n)
                . appendPathStep (IntoConstructor name)
      in  K . addInfo $ d x y

gDiffProductInfix ::
  ConstructorName ->
  NP Differ [x, y] ->
  NP I [x, y] ->
  NP I [x, y] ->
  DiffResult
gDiffProductInfix = error "todo"

gDiffProductRecord ::
  -- (SListI xs) =>
  ConstructorName ->
  NP FieldInfo xs ->
  NP Differ xs ->
  NP I xs ->
  NP I xs ->
  DiffResult
gDiffProductRecord name fs ds xs ys = mconcat . collapse_NP $ hzipWith4 something fs ds xs ys
  where
    something :: forall a. FieldInfo a -> Differ a -> I a -> I a -> K DiffResult a
    something fInfo (Differ d) (I x) (I y) =
      let addInfo =
            overPath $
              appendPathStep (IntoNamedField $ fieldName fInfo)
                . appendPathStep (IntoConstructor name)
      in  K . addInfo $ d x y

gDiffNewtype ::
  forall x.
  Differ x ->
  ConstructorInfo '[x] ->
  x ->
  x ->
  DiffResult
gDiffNewtype (Differ d) info x y =
  let addInfo =
        overPath $
          appendPathStep field . appendPathStep cstr
      cstr = IntoConstructor $ constructorName info
      field = case info of
        Constructor {} -> IntoUnnamedField 0
        Record _ fInfo -> IntoNamedField $ fieldName $ unNP fInfo
  in  addInfo $ d x y

------------------------------------------------------------
-- Util

compareNS' ::
  forall r f g stuff zs.
  (All SListI zs) =>
  -- | what to do if first is smaller
  (forall x ys. stuff x -> f x -> NS (stuff `Product` g) ys -> r) ->
  -- | what to do if both are equal
  (forall x. (SListI x) => stuff x -> f x -> g x -> r) ->
  -- | what to do if first is larger
  (forall xs y. stuff y -> NS (stuff `Product` f) xs -> g y -> r) ->
  NP stuff zs ->
  NS f zs ->
  NS g zs ->
  r
compareNS' lt eq gt = go
  where
    go :: forall ys. (All SListI ys) => NP stuff ys -> NS f ys -> NS g ys -> r
    go i (Z x) (Z y) = eq (hd i) x y
    go i (Z x) (S ys) = lt (hd i) x (hzipWith Pair (tl i) ys)
    go i (S xs) (Z y) = gt (hd i) (hzipWith Pair (tl i) xs) y
    go (_ :* is) (S xs) (S ys) = go is xs ys

unNP :: NP f '[a] -> f a
unNP = hd

tailConstructorInfo :: ConstructorInfo (x ': xs) -> ConstructorInfo xs
tailConstructorInfo = \case
  Constructor cName -> Constructor cName
  Infix cName _ _ -> Constructor cName
  Record cName (_ :* recs) -> Record cName recs
