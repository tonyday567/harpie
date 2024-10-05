harpie
===

[![Hackage](https://img.shields.io/hackage/v/harpie.svg)](https://hackage.haskell.org/package/harpie)
[![Build Status](https://github.com/tonyday567/harpie/workflows/build/badge.svg)](https://github.com/tonyday567/harpie/actions) <img src=https://static.wikia.nocookie.net/monster/images/2/28/Harpy.jpg/revision/latest?cb=20110129191316 alt="harpie" width="100"/>


Haskell array programming.

**harpie** is an array programming library written in Haskell:

- It provides for arrays where shape is specified and manipulated at either the value- or type- level, and provides a similar API for both situations.

- It is rank polymorphic. Vectors, Matrices, Tensors or Scalars all use the same functions.

- It is dimensionally agnostic: the same function can be applied over rows, columns or over multiple dimensions.

- It aims to promote a higher-kinded ergonomic style of array programming, in idiomatic Haskell style.

The library is experimental and educational (at least for the authors) and likely to remain so. Collaboration is welcome.

Usage
===

Naming conventions clash with the prelude and with each other, so importing should be qualified.

``` haskell
import qualified Harpie.Array as A
import qualified Harpie.Shape as S
import qualified Harpie.Fixed as F

-- >>> a = F.range @[2,3,4]
-- >>> F.shape a
-- [2,3,4]
-- >>> pretty a
-- [[[0,1,2,3],
--   [4,5,6,7],
--   [8,9,10,11]],
--  [[12,13,14,15],
--   [16,17,18,19],
--   [20,21,22,23]]]

-- >>> a = A.range [2,3,4]
-- >>> F.shape a
-- [2,3,4]
-- >>> pretty a
-- [[[0,1,2,3],
--   [4,5,6,7],
--   [8,9,10,11]],
--  [[12,13,14,15],
--   [16,17,18,19],
--   [20,21,22,23]]]
```

Design notes
===

Haskell utility
---

The library attempts to be idiomatic Haskell and otherwise fit in with the ecosystem at time of writing. In particular, boxed Vectors are used as the array container to enforce immutability, permit lazy expression, and allow arbitrary element types.

Consistency of type- and value- level list algorithms.
---

The library is an attempt to provide a consistent approach to array programming whether or not array shape is held and computed at value-level or at type-level. 

The Harpie.Shape module contains common list algorithms for value-level shapes (ie Int list operatorions) and type-level shapes (a type-level Nat list operations) that is as close to the same as possible. They cannot be identical because type and value programming in Haskell are very different languages. The (first-class-families)[https://hackage.haskell.org/package/first-class-families] library was used to achieve this.

Is it safe?
---

'Harpie.Fixed' arrays sit at around level 4.5 in (Justin Le's type safety heirarchy)[https://blog.jle.im/entry/levels-of-type-safety-haskell-lists.html]. They are designed with static type-safety in mind, a run-time error in shape or index computation is a bug. Typed shape information, however, is modelled on GHC.TypeNats with Nat and thus [Nat] being opaque types rather than structured peano numbers. This makes compiler KnownNat and KnownNats discovery and proof witnessing problematic.

Instead of dependently-typed approaches, the library leans into switching from fixed to untyped shape representation if shape is not known at runtime. In the experience of writing this iteration of the library, it became obvious that type-level programming has it's own share of bugs and has trade-offs versus the much richer language of Haskell value-level programming.

Is it fast?
---

Maybe. Does [vector](https://hackage.haskell.org/package/vector) streaming lead to efficient code? If it does then harpie should be able to access this efficiency.

There are plenty of faster array programming options out there, in Haskell and elsewhere. If you need close-to-the-metal performance, try:

- [hmatrix](https://hackage.haskell.org/package/hmatrix) provides bindings to [BLAS](https://en.wikipedia.org/wiki/Basic_Linear_Algebra_Subprograms) and 45 years of Fortran tweaking.
- [hasktorch](https://github.com/hasktorch/hasktorch) or [tensorflow](https://hackage.haskell.org/package/tensorflow) bindings to the might of the Python machince learning community.
- [massive](https://hackage.haskell.org/package/massiv) or  [accelerate](https://hackage.haskell.org/package/accelerate) for homegrown Haskell speed.
- [repa](https://hackage.haskell.org/package/repa) for archeology.
- [orthotope](https://hackage.haskell.org/package/orthotope) for a more APL treatment of the problem domain, and for unboxed vector usage. 


backpermute FTW
---

As computational complexity increases, either in array programmimg or in connectivity to other problem domains, harpie performance is (one-day) expected to come into her own.

A canonical backpermute function is detailed in [Regular, Shape-polymorphic, Parallel Arrays in Haskell](https://benl.ouroborus.net/papers/2010-rarrays/repa-icfp2010.pdf) and would be implemented in harpie as:

-- > repa_backpermute f a = tabulate (f (shape a)) (index a . f)

The harpie backpermute splits the function application into two parts, the shape manipulation and the array manipulation:

-- > harpie_backpermute f g a = tabulate (f (shape a)) (index a . g)

This, more general specification, should (one-day) allow more opportunities for the fusion rule to kick in:

-- > forall f f' g g' (a :: forall a. Array a)). backpermute f g (backpermute f' g' a) == backpermute (f . f') (g . g') a

Probelms
===

- type errors and information quickly explode in complexity and types can run to hundreds of lines. It's quite ugly.

- Constraints cannot be encapsulated in the function they originate and thus have to be propogated in line with usage. Tyep-level programming in Haskell is a good idea in search of some composition.


