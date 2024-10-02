harry
===

[![Hackage](https://img.shields.io/hackage/v/harry.svg)](https://hackage.haskell.org/package/harry)
[![Build Status](https://github.com/tonyday567/harry/workflows/haskell-ci/badge.svg)](https://github.com/tonyday567/harry/actions?query=workflow%3Ahaskell-ci)

=harry= is a [hyper-rectangular](https://en.wikipedia.org/wiki/Hyperrectangle) array library written in Haskell.

- Boxed Vectors are used as the array container to enforce immutability, permit lazy expression, allow arbitrary element types and otherwise enjoy full usage of the wider Haskell ecosystem.
- An Array type with a value-level Shape is provided in Harry.Array and an Array type with a type-level Shape is provided in Harry.Fixed. With a few exceptions, the API for both modules is the same.
- A distinguishing feature of the library is that functions are =dimension polymorphic=. Most operators input the dimension or dimensions to be operated on or along.

Usage
===

Naming conventions clash with the prelude and with each other, so importing should be qualified.

``` haskell
import qualified Harry.Array as A
import qualified Harry.Shape as S
import qualified Harry.Fixed as F
```

Design notes
===

backpermute

Start with dynamic

What are arrays

Boxes of stuff

Ergonomic

Is it fast?
- maybe
- wrapper around vector streaming
- backpermute a novel lazy-fusion mechanism

Type safety
- modelled on GHC.TypeNats
- favours arrays as contiguous memory (and not peanoized artifacts)
- 4.5 on (Justin Le's levels)[https://blog.jle.im/entry/levels-of-type-safety-haskell-lists.html] - Static Type safety but no witnesses or dependent typing ambition.


