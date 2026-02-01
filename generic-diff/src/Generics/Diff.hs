{-# OPTIONS_GHC -Wno-partial-fields #-}

{- | Generic detailed comparisons, without boilerplate.

The simplest way in Haskell that we can use to compare two values of the same type is using 'Eq'.
The '(==)' operator gives us a simple 'Bool'ean response: 'True' if the values are equal, or 'False'
otherwise.
Slightly more informative is 'Ord': the 'compare' function (or operators '(<=)', '(>)' etc) tells
us, if two values are different, which one can be considered as "less than" or "more than" the other.

One situation in which these might not be enough is testing. Say you have a pair of (de)serialisation
functions (which we'll imagine are total, for simplicity), and which we expect to be inverses:

@
serialise   :: (Serialise a) => a      -> String
deserialise :: (Serialise a) => String -> a
@

Let's imagine we have some ([Hspec](https://hackage.haskell.org/package/hspec/docs/Test-Hspec.html))
tests, e.g.:

@
unitTest :: (Serialise a) => a -> Spec
unitTest value =
  let serialised = serialise value
      deserailised = deserialise serialised
  in  it "Serialisation should satisfy the round-trip property" $ deserialised \`shouldBe\` value
@

What happens if the test fails? If we're dealing with a simple type like 'Int', the error will be very
easy to debug:

@
  1) Serialisation should satisfy the round-trip property
       expected: 2
        but got: 1
@

But what if our type is much more complicated, with lots of constructors, fields, nesting...
Especially if the type has a derived show instance, the output can be very dense, littered with
parentheses, and overall very difficult to play "spot the difference" with.

That's where this library comes in handy. Using the 'Diff' typeclass, we can identify precisely
where two values differ. A "top-level" diff will tell you at which constructor and which field the values
are different; if the types of those fields also have useful 'Diff' instances, we can recurse into them to
pinpoint the error even more accurately. Even better, we can derive our 'Diff' instances completely
automatically as long as our types have instances for 'Generics.SOP.Generic' (and
'Generics.SOP.HasDatatypeInfo') from [generics-sop](https://hackage.haskell.org/package/generics-sop).
In fact, we use the types 'Generics.SOP.NP' and 'Generics.SOP.NS' extensively to define the types we
use for representing diffs.

= Understanding

To aid understanding (both for this library and for @generics-sop@), I think it helps to think of an ADT
as a grid:

@
data MyType
  = Con1 Char (Either Int String)
  | Con2 [Bool]
@

This corresponds to a grid with one row per constructor, one column per field:

+----------------+---------------+-------------------+
| Constructor    |             Fields                |
+================+===============+===================+
| Con1           |   Char        | Either Int String |
+----------------+---------------+-------------------+
| Con2           |   [Bool]      |    N / A          |
+----------------+---------------+-------------------+

A value of type @MyType@ can be thought of as being one row of the grid, with one value for each
column in the row.

Now, if we have two values of type @MyType@, there's two main ways they can differ. If they inhabit
different rows of the grid, they're clearly not equal, and all we can say is "this one uses this
constructor, that one uses that constructor" (see 'WrongConstructor'). In other words we just report the
names of the two rows.
If they inhabit the same row of the grid, we have to go column by column, comparing each pair of values (we'll
get to how we compare them in a second).
If they're all pairwise equal, we can conclude the two values are 'Equal'; if one pair fails the comparison
test, we stop checking and say "the types differ at this constructor, in this field" (see 'FieldMismatch').
Effectively, we point to a single cell of the grid and say "that's where the inequality is".

You might note that this process is very similar to how a stock-derived 'Eq' instance works. All we've added
so far is an extra dimension to the output, detailing not just that two values differ, but where in the grid
they differ. Where things get interesting is how we do the pairwise comparison between fields. If the
comparison test is '(==)', then it's as above: we find out that two values are either equal, or not equal; and
if they're not equal, we find out at which field that inequality happens. However, as in @MyType@ above, types
often refer to other types! @Either Int String@ also has its own grid:

@
-- excuse the pseudo-Haskell
type Either Int String
  = Left Int
  | Right String
@

Similar to above, we have:

+----------------+---------------+
| Constructor    | Fields        |
+================+===============+
| Left           |   Int         |
+----------------+---------------+
| Right          |   String      |
+----------------+---------------+

In fact, if we squint a bit, this grid actually exists __inside__ the cell of the @MyType@ grid:

+----------------+---------------+-------+--------+
|                |               | Left  | Int    |
| Con1           |   Char        +-------+--------+
|                |               | Right | String |
+----------------+---------------+-------+--------+
| Con2           |   [Bool]      |    N / A       |
+----------------+---------------+----------------+

This gives us an extra level of granularity: when we get to the pair of @Either Int String@ fields, rather than
just delegating to 'Eq', we can go through the same procedure as above. Then instead of "the two values differ at the
@Either Int String@ field of the @Con1@ constructor", we can say "the two values differ at the @Either Int String@
field of the @Con1@ constructor, and those two field differs because one uses the @Left@ constructor and the other
uses the @Right@ constructor"! And of course, once we have one step of recursion, we have as many as we want...

== Implementing instances

The 'Diff' class encapsulates the above behaviour with 'diff'. It's very strongly recommended that you don't
implement 'diff' yourself, but use the default implementation using 'Generics.SOP.Generic', which is just 'gdiff'.
In case you might want to implement 'diff' yourself, there are three other functions you might want to use.

- 'eqDiff' simply delegates the entire process to '(==)', and will only ever give 'Equal' or 'TopLevelNotEqual'. This is
no more useful than 'Eq', and should only be used for primitive types (e.g. all numeric types like 'Char' and 'Int')
use 'eqDiff', since they don't really have ADTs or recursion.

- 'gdiffTopLevel' does the above process, but without recursion. In other words each pair of fields is compared using
'(==)'. This is definitely better than 'Eq', by one "level". One situation when this might be useful is when your
type refers to types from other libraries, and you want to avoid orphan 'Diff' instances for those types. Another
is when the types of the fields are "small" enough that we don't care about recursing into them. For example:

@
data HttpVersion
  = Http1
  | Http2
  deriving (Eq)

data Request = Request
  { host :: String
  , port :: Int
  -- there's no instance of 'Diff' for @Map@, so just compare for equality using '(==)'
  , parameters :: Map String String
  -- 'Diff' doesn't really add anything over 'Eq' for enum types, so 'Eq' is fine
  , httpVersion :: HttpVersion
  }
  deriving stock (GHC.Generics.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

instance 'Diff' Request where
  'diff' = 'gdiffTopLevel'
@

- 'diffWithSpecial' lets us handle edge cases for funky types with unusual 'Eq' instances or preserved
invariants. See "Generics.Diff.Special".

For completeness, we also provide one more implementation function: 'gdiffWith' lets you provide a set of
'Differ's (comparison functions) to use for each pair of fields (one per cell of the grid).
I'm not sure in what situation you'd want this, but there you go.
-}
module Generics.Diff
  ( -- * Class
    Diff (..)

    -- ** Implementing diff
  , gdiff
  , gdiffTopLevel
  , gdiffWith
  , eqDiff

    -- * Types
  , DiffResult (..)
  , DiffError (..)
  , DiffErrorNested (..)
  , ListDiffError (..)
  , DiffAtField (..)
  , (:*:) (..)
  , Differ (..)
  )
where

import Generics.Diff.Class
import Generics.Diff.Instances ()
import Generics.Diff.Type
