{- | The types in 'Generic.Diff' have derived 'Show' instances that don't help at all in
one of the goals for the library, which is readability. This module lets us render those types
in a friendly way.
-}
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

    -- * Intermediate representation
  , Doc (..)
  , diffErrorDoc
  , renderDoc
  , showR
  , linesDoc
  , makeDoc
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.SOP.NS
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.IO as TL
import Generics.Diff.Type
import Generics.SOP as SOP

-- | Sensible rendering defaults. No numbers, 2-space indentation.
defaultRenderOpts :: RenderOpts
defaultRenderOpts =
  RenderOpts
    { indentSize = 2
    , numberedLevels = False
    }

-- | Print a 'DiffResult' to the terminal.
printDiffResult :: DiffResult a -> IO ()
printDiffResult = printDiffResultWith defaultRenderOpts

-- | Print a 'DiffResult' to the terminal, using custom 'RenderOpts'.
printDiffResultWith :: RenderOpts -> DiffResult a -> IO ()
printDiffResultWith opts =
  TL.putStrLn . TB.toLazyText . renderDiffResultWith opts

-- | Render a 'DiffResult' using a lazy 'TB.Builder'.
renderDiffResult :: DiffResult a -> TB.Builder
renderDiffResult = renderDiffResultWith defaultRenderOpts

-- | Render a 'DiffResult' using a lazy 'TB.Builder', using custom 'RenderOpts'.
renderDiffResultWith :: RenderOpts -> DiffResult a -> TB.Builder
renderDiffResultWith opts = renderDoc opts 0 . diffResultDoc

-- | Render a 'DiffError' using a lazy 'TB.Builder'.
renderDiffError :: DiffError a -> TB.Builder
renderDiffError = renderDiffErrorWith defaultRenderOpts

-- | Render a 'DiffError' using a lazy 'TB.Builder', using custom 'RenderOpts'.
renderDiffErrorWith :: RenderOpts -> DiffError a -> TB.Builder
renderDiffErrorWith opts = renderDoc opts 0 . diffErrorDoc

-- | Render a 'DiffErrorNested' using a lazy 'TB.Builder'.
renderDiffErrorNested :: DiffErrorNested xss -> TB.Builder
renderDiffErrorNested = renderDiffErrorNestedWith defaultRenderOpts

-- | Render a 'DiffErrorNested' using a lazy 'TB.Builder', using custom 'RenderOpts'.
renderDiffErrorNestedWith :: RenderOpts -> DiffErrorNested xss -> TB.Builder
renderDiffErrorNestedWith opts = renderDoc opts 0 . diffErrorNestedDoc

-- | Render a 'ListDiffError' using a lazy 'TB.Builder'.
renderListDiffError :: ListDiffError xss -> TB.Builder
renderListDiffError = renderListDiffErrorWith defaultRenderOpts

-- | Render a 'ListDiffError' using a lazy 'TB.Builder', using custom 'RenderOpts'.
renderListDiffErrorWith :: RenderOpts -> ListDiffError xss -> TB.Builder
renderListDiffErrorWith opts = renderDoc opts 0 . listDiffErrorDoc "list"

------------------------------------------------------------
-- Doc representation
-- Rendering a 'DiffResult' happens in two steps: converting our strict SOP types into a much simpler
-- intermediate representation, and then laying them out in a nice way.

-- | Create a 'Doc' with a non-empty list of lines and a nested error.
makeDoc :: NonEmpty TB.Builder -> DiffError a -> Doc
makeDoc ls err = Doc ls (Just $ diffErrorDoc err)

-- | Create a simple 'Doc' without a nested error.
linesDoc :: NonEmpty TB.Builder -> Doc
linesDoc ls = Doc ls Nothing

diffResultDoc :: DiffResult a -> Doc
diffResultDoc = \case
  Equal -> linesDoc (pure "Equal")
  Error err -> diffErrorDoc err

-- | Convert a 'DiffError' to a 'Doc'.
diffErrorDoc :: DiffError a -> Doc
diffErrorDoc = \case
  TopLevelNotEqual -> linesDoc (pure "Not equal")
  Nested err -> diffErrorNestedDoc err
  DiffList listErr -> listDiffErrorDoc "list" listErr
  DiffNonEmpty listErr -> listDiffErrorDoc "non-empty list" listErr

listDiffErrorDoc :: TB.Builder -> ListDiffError a -> Doc
listDiffErrorDoc lst = \case
  DiffAtIndex idx err ->
    let lns = pure $ "Diff at " <> lst <> " index " <> showR idx <> " (0-indexed)"
    in  makeDoc lns err
  WrongLengths l r ->
    linesDoc $
      "Lists are wrong lengths"
        :| [ "Length of left list: " <> showR l
           , "Length of right list: " <> showR r
           ]

diffErrorNestedDoc :: DiffErrorNested xss -> Doc
diffErrorNestedDoc = \case
  WrongConstructor l r ->
    let cName = collapse_NS . liftANS (K . constructorNameR)
        lCons = cName l
        rCons = cName r
    in  linesDoc $
          "Wrong constructor"
            :| [ "Constructor of left value: " <> lCons
               , "Constructor of right value: " <> rCons
               ]
  FieldMismatch (DiffAtField ns) ->
    let (cName, fieldLoc, err) =
          collapse_NS $
            liftANS (\(cInfo :*: nsErr) -> K (unpackAtLocErr cInfo nsErr)) ns
        lns =
          ("Both values use constructor " <> cName <> " but fields don't match")
            :| [renderRField fieldLoc <> ":"]
    in  Doc lns (Just err)

{- | Render a 'Doc' as a text 'TB.Builder'. This should be the only way we escape a 'Doc'.

The output can be configured using 'RenderOpts'.
-}
renderDoc :: RenderOpts -> Int -> Doc -> TB.Builder
renderDoc opts ind = unlinesB . go ind
  where
    go n Doc {..} =
      let otherIndent = mkIndent opts False n
          firstIndent = mkIndent opts True n
          l :| ls = docLines
          firstLine = firstIndent <> l
          otherLines = [otherIndent <> line | line <- ls]
          allLines = firstLine : otherLines
      in  case docSubDoc of
            Nothing -> allLines
            Just err -> allLines <> go (n + 1) err

type RConstructorName = TB.Builder

type RFieldName = TB.Builder

data InfixSide = ILeft | IRight

data RField
  = IdxField Int
  | InfixField InfixSide
  | RecordField RFieldName

constructorNameR :: ConstructorInfo xs -> RConstructorName
constructorNameR = \case
  Constructor name -> TB.fromString name
  Infix name _ _ -> "(" <> TB.fromString name <> ")"
  Record name _ -> TB.fromString name

unpackAtLocErr :: forall xs. ConstructorInfo xs -> NS DiffError xs -> (RConstructorName, RField, Doc)
unpackAtLocErr cInfo nsErr =
  let err = collapse_NS $ liftANS (K . diffErrorDoc) nsErr
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

renderRField :: RField -> TB.Builder
renderRField = \case
  IdxField n -> "In field " <> showR n <> " (0-indexed)"
  InfixField side -> case side of
    ILeft -> "In the left-hand field"
    IRight -> "In the right-hand field"
  RecordField fName -> "In field " <> fName

------------------------------------------------------------
-- Util

unlinesB :: [TB.Builder] -> TB.Builder
unlinesB (b : bs) = b <> TB.singleton '\n' <> unlinesB bs
unlinesB [] = mempty

-- | 'show' a value as a 'TB.Builder'.
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
