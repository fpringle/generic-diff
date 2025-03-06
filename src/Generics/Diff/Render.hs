module Generics.Diff.Render
  ( -- * Rendering
    renderDiffResult
  , renderDiffResultWith

    -- * Printing
  , printDiffResult
  , printDiffResultWith

    -- * Options
  , RenderOpts
  , defaultRenderOpts
  , indentSize
  , numberedLevels

    -- * Helper rendering functions
  , renderDiffError
  , renderDiffErrorWith
  , renderDiffErrorNested
  , renderDiffErrorNestedWith
  , renderListDiffError
  , renderListDiffErrorWith
  )
where

import Data.SOP.NS
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.IO as TL
import Generics.Diff.Type
import Generics.SOP as SOP
import Numeric.Natural

data RenderOpts = RenderOpts
  { indentSize :: Natural
  , numberedLevels :: Bool
  }
  deriving (Show)

defaultRenderOpts :: RenderOpts
defaultRenderOpts =
  RenderOpts
    { indentSize = 2
    , numberedLevels = False
    }

printDiffResult :: DiffResult a -> IO ()
printDiffResult = printDiffResultWith defaultRenderOpts

printDiffResultWith :: RenderOpts -> DiffResult a -> IO ()
printDiffResultWith opts =
  TL.putStrLn . (<> "\n") . TB.toLazyText . renderDiffResultWith opts

renderDiffResult :: DiffResult a -> TB.Builder
renderDiffResult = renderDiffResultWith defaultRenderOpts

renderDiffResultWith :: RenderOpts -> DiffResult a -> TB.Builder
renderDiffResultWith opts = renderRDiffResultWith opts . diffResultR

renderDiffError :: DiffError a -> TB.Builder
renderDiffError = renderDiffErrorWith defaultRenderOpts

renderDiffErrorWith :: RenderOpts -> DiffError a -> TB.Builder
renderDiffErrorWith opts = renderRDiffErrorWith opts 0 . diffErrorR

renderDiffErrorNested :: DiffErrorNested xss -> TB.Builder
renderDiffErrorNested = renderDiffErrorNestedWith defaultRenderOpts

renderDiffErrorNestedWith :: RenderOpts -> DiffErrorNested xss -> TB.Builder
renderDiffErrorNestedWith opts = renderRDiffErrorNested opts 0 . diffErrorNestedR

renderListDiffError :: ListDiffError xss -> TB.Builder
renderListDiffError = renderListDiffErrorWith defaultRenderOpts

renderListDiffErrorWith :: RenderOpts -> ListDiffError xss -> TB.Builder
renderListDiffErrorWith opts = renderRListDiffError opts "list" 0 . listDiffErrorR

------------------------------------------------------------
-- Intermediate representation

type RConstructorName = TB.Builder

type RFieldName = TB.Builder

data RDiffResult
  = RError RDiffError
  | REqual

data InfixSide = ILeft | IRight

data RField
  = IdxField Int
  | InfixField InfixSide
  | RecordField RFieldName

data RDiffErrorNested
  = RWrongConstructor RConstructorName RConstructorName
  | RFieldMismatch RConstructorName RField RDiffError

data RDiffError where
  RTopLevelNotEqual :: RDiffError
  RNested :: RDiffErrorNested -> RDiffError
  RDiffList :: RListDiffError -> RDiffError
  RDiffNonEmpty :: RListDiffError -> RDiffError

data RListDiffError
  = RDiffAtIndex Int RDiffError
  | RWrongLengths Int Int

diffResultR :: DiffResult a -> RDiffResult
diffResultR = \case
  Equal -> REqual
  Error err -> RError $ diffErrorR err

diffErrorR :: DiffError a -> RDiffError
diffErrorR = \case
  TopLevelNotEqual -> RTopLevelNotEqual
  Nested nested -> RNested $ diffErrorNestedR nested
  DiffList list -> RDiffList $ listDiffErrorR list
  DiffNonEmpty list -> RDiffNonEmpty $ listDiffErrorR list

diffErrorNestedR :: DiffErrorNested a -> RDiffErrorNested
diffErrorNestedR = \case
  WrongConstructor l r ->
    let cName = collapse_NS . liftANS (K . constructorNameR)
    in  RWrongConstructor (cName l) (cName r)
  FieldMismatch (DiffAtField ns) ->
    let (cName, fieldLoc, err) =
          collapse_NS $
            liftANS (\(cInfo :*: nsErr) -> K (unpackAtLocErr cInfo nsErr)) ns
    in  RFieldMismatch cName fieldLoc err

constructorNameR :: ConstructorInfo xs -> RConstructorName
constructorNameR = \case
  Constructor name -> TB.fromString name
  Infix name _ _ -> "(" <> TB.fromString name <> ")"
  Record name _ -> TB.fromString name

unpackAtLocErr :: forall xs. ConstructorInfo xs -> NS DiffError xs -> (RConstructorName, RField, RDiffError)
unpackAtLocErr cInfo nsErr =
  let err = collapse_NS $ liftANS (K . diffErrorR) nsErr
  in  case cInfo of
        Constructor name -> (TB.fromString name, IdxField $ index_NS nsErr, err)
        Infix name _ _ ->
          let side = case nsErr of
                Z _ -> ILeft
                S _ -> IRight
          in  ("(" <> TB.fromString name <> ")", InfixField side, err)
        Record name fields ->
          let fName = collapse_NS $ liftANS (K . TB.fromString . fieldName) $ pickOut fields nsErr
          in  (TB.fromString name, RecordField fName, err)

listDiffErrorR :: ListDiffError a -> RListDiffError
listDiffErrorR = \case
  DiffAtIndex idx err -> RDiffAtIndex idx $ diffErrorR err
  WrongLengths l r -> RWrongLengths l r

renderRDiffResultWith :: RenderOpts -> RDiffResult -> TB.Builder
renderRDiffResultWith opts = \case
  REqual -> "Equal"
  RError err -> renderRDiffErrorWith opts 0 err

renderRDiffErrorWith :: RenderOpts -> Int -> RDiffError -> TB.Builder
renderRDiffErrorWith opts ind = \case
  RTopLevelNotEqual -> firstIndent <> "Not equal"
  RNested den -> renderRDiffErrorNested opts ind den
  RDiffList listErr -> renderRListDiffError opts "list" ind listErr
  RDiffNonEmpty listErr -> renderRListDiffError opts "non-empty list" ind listErr
  where
    firstIndent = mkIndent opts True ind

renderRListDiffError :: RenderOpts -> TB.Builder -> Int -> RListDiffError -> TB.Builder
renderRListDiffError opts lst ind = \case
  RDiffAtIndex idx err ->
    (firstIndent <> "Diff at " <> lst <> " index " <> showR idx <> " (0-indexed)\n")
      <> renderRDiffErrorWith opts (ind + 1) err
  RWrongLengths l r ->
    (firstIndent <> "Lists are wrong lengths\n")
      <> (otherIndent <> "Length of left list: " <> showR l <> "\n")
      <> (otherIndent <> "Length of right list: " <> showR r)
  where
    otherIndent = mkIndent opts False ind
    firstIndent = mkIndent opts True ind

renderRDiffErrorNested :: RenderOpts -> Int -> RDiffErrorNested -> TB.Builder
renderRDiffErrorNested opts ind = \case
  RWrongConstructor lCons rCons ->
    (firstIndent <> "Wrong constructor\n")
      <> (otherIndent <> "Constructor of left value: " <> lCons <> "\n")
      <> (otherIndent <> "Constructor of right value: " <> rCons)
  RFieldMismatch cName fieldLoc err ->
    (firstIndent <> "Both values use constructor " <> cName <> " but fields don't match\n")
      <> (otherIndent <> renderRField fieldLoc <> ":\n" <> renderRDiffErrorWith opts (ind + 1) err)
  where
    firstIndent = mkIndent opts True ind
    otherIndent = mkIndent opts False ind

renderRField :: RField -> TB.Builder
renderRField = \case
  IdxField n -> "In field " <> showR n <> " (0-indexed)"
  InfixField side -> case side of
    ILeft -> "In the left-hand field"
    IRight -> "In the right-hand field"
  RecordField fName -> "In field " <> fName

------------------------------------------------------------
-- Util

showR :: (Show a) => a -> TB.Builder
showR = TB.fromString . show
{-# INLINE showR #-}

liftANS :: forall f g xs. (forall a. f a -> g a) -> NS f xs -> NS g xs
liftANS f = go
  where
    go :: forall ys. NS f ys -> NS g ys
    go = \case
      Z z -> Z (f z)
      S s -> S (go s)

mkIndent :: RenderOpts -> Bool -> Int -> TB.Builder
mkIndent RenderOpts {..} isFirst ind =
  let spaces = TB.fromText (T.replicate (ind * fromIntegral indentSize) " ")
      number = showR (ind + 1) <> ". "
      noNumber = "   "

      withNumber = spaces <> number
      withoutNumber = spaces <> noNumber
  in  if numberedLevels
        then if isFirst then withNumber else withoutNumber
        else spaces
