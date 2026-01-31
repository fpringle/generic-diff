[![Haskell CI](https://github.com/fpringle/generic-diff/actions/workflows/haskell.yml/badge.svg)](https://github.com/fpringle/generic-diff/actions/workflows/haskell.yml)

# `generic-diff` instances

The [generic-diff](https://hackage.haskell.org/package/generic-diff) package
aims to be lightweight and not force any instances which might have more than
one interpretation.

This package provides a more comprehensive set of instances for types from a
range of common packages.

Currently we provide instances for [Map](https://hackage-content.haskell.org/package/containers-0.8/docs/Data-Map-Lazy.html#t:Map), [Seq](https://hackage-content.haskell.org/package/containers-0.8/docs/Data-Sequence.html#t:Seq), [Set](https://hackage-content.haskell.org/package/containers-0.8/docs/Data-Set.html#t:Set) and [Tree](https://hackage-content.haskell.org/package/containers-0.8/docs/Data-Tree.html#t:Tree) from the containers package.
