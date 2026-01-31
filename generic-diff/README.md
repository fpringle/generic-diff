# Generic structural diffs

`generic-diff` lets us pinpoint exactly where two values differ, which can be very useful, for example for debugging failing tests.
This functionality is provided by the `Diff` typeclass, for which instances can be derived automatically using `Generic` from
[generics-sop](https://github.com/well-typed/generics-sop).

For detailed information, see the [Hackage docs](https://hackage.haskell.org/package/generic-diff/docs/Generics-Diff.html).

## Example

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import Generics.Diff
import Generics.Diff.Render

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

expr1, expr2 :: Expr
expr1 = Bin (Atom 1) Plus (Bin (Atom 1) Plus (Atom 1))
expr2 = Bin (Atom 1) Plus (Bin (Atom 1) Plus (Atom 2))
```

```haskell
ghci> printDiffResult $ diff expr1 expr2
In field right:
  Both values use constructor Bin but fields don't match
  In field right:
    Both values use constructor Atom but fields don't match
    In field 0 (0-indexed):
      Not equal
```
