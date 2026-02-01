{- | 'SpecialDiff' lets us define diff types for edge cases. For example, say we want to use
a type like 'ListDiffError' to diff lists in "one go", rather than recursing into a level of
SOP for each new element we examine.

Let's take a look at the implementation for lists:

@
data 'ListDiffError' a
  = 'DiffAtIndex' Int ('DiffError' a)   -- there's a diff between two elements at this index
  | 'WrongLengths' Int Int            -- one list is a (strict) prefix of the other

instance ('Generics.Diff.Diff' a) => 'SpecialDiff' [a] where
  type 'SpecialDiffError' [a] = 'ListDiffError' a
  'specialDiff' = 'diffListWith' 'Generics.Diff.diff'
  'renderSpecialDiffError' = 'Generics.Diff.Render.listDiffErrorDoc' "list"

'diffListWith' :: (a -> a -> 'DiffResult' a) -> [a] -> [a] -> Maybe ('ListDiffError' a)
'diffListWith' d = go 0
  where
    -- we compare each element pairwise.
    go ::
      -- current index
      Int ->
      -- remaining input lists
      [a] -> [a] ->
      Maybe ('ListDiffError' a)

    -- base case: if we've reach the end of both lists, they're equal, return Nothing
    go _ [] [] = Nothing

    -- if we reach the end of one list first, return a 'WrongLengths'
    go n [] ys = Just $ 'WrongLengths' n (n + length ys)
    go n xs [] = Just $ 'WrongLengths' (n + length xs) n

    -- recursive step: comparing the two head elements using the provider differ
    go n (x : xs) (y : ys) = case d x y of
      'Equal' ->
        -- the head elements are equal, recurse
        go (n + 1) xs ys
      'Error' err ->
        -- the head elements are not equal, return the error with the index
        Just $ 'DiffAtIndex' n err

-- To construct a 'Doc' we need some lines at the top, and optionally a sub-error.
'Generics.Diff.Render.listDiffErrorDoc' :: 'TB.Builder' -> 'ListDiffError' a -> 'Doc'
'Generics.Diff.Render.listDiffErrorDoc' lst = \case
  'DiffAtIndex' idx err ->
    let
      -- top line
      lns = pure $ "Diff at " <> lst <> " index " <> 'Generics.Diff.Render.showB' idx <> " (0-indexed)"
    in
      -- 'Generics.Diff.Render.makeDoc' is a smart constructor for a 'Doc' with a sub error
      'Generics.Diff.Render.makeDoc' lns err
  'WrongLengths' l r ->
      -- 'Generics.Diff.Render.linesDoc' is a smart constructor for a 'Doc' without a sub error
    'Generics.Diff.Render.linesDoc' $
      (lst <> "s are wrong lengths")
        :| [ "Length of left list: " <> 'Generics.Diff.Render.showB' l
           , "Length of right list: " <> 'Generics.Diff.Render.showB' r
           ]
@

Note that 'diffListWith' and 'Generics.Diff.Render.listDiffErrorDoc' are exported functions, rather than
written inline, because there are other list-like types which will have almost identical instances and can
reuse the code. For example, the implementation of 'SpecialDiff' for 'NE.NonEmpty' lists is:

@
instance ('Generics.Diff.Diff' a) => 'SpecialDiff' ('NE.NonEmpty' a) where
  type 'SpecialDiffError' ('NE.NonEmpty' a) = 'ListDiffError' a
  'specialDiff' l r = 'diffListWith' 'Generics.Diff.diff' ('NE.toList' l) ('NE.toList' r)
  'renderSpecialDiffError' = 'Generics.Diff.Render.listDiffErrorDoc' "non-empty list"
@
-}
module Generics.Diff.Special
  ( SpecialDiff (..)
  , diffWithSpecial
  , gspecialDiffNested

    -- * Lists
  , module List
  )
where

import Generics.Diff.Class
import Generics.Diff.Special.List as List
import Generics.Diff.Type
