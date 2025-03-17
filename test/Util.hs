module Util where

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Generics.Diff
import Generics.Diff.Instances ()
import Generics.Diff.Render
import qualified Test.QuickCheck as Q

propDiffResult :: (Diff a, Show a) => a -> a -> DiffResult a -> Q.Property
propDiffResult leftValue rightValue expectedDiffResult =
  let actualDiffResult = diff leftValue rightValue
      eq = expectedDiffResult == actualDiffResult
      showDiffResult = TL.unpack . TB.toLazyText . renderDiffResult
      addLabel =
        if eq
          then Q.property
          else
            Q.counterexample ("Expected DiffResult:\n" <> showDiffResult expectedDiffResult)
              . Q.counterexample ("Actual DiffResult:\n" <> showDiffResult actualDiffResult)
              . Q.counterexample ("Left value:\n" <> show leftValue)
              . Q.counterexample ("Right value:\n" <> show rightValue)
  in  addLabel eq
