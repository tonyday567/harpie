0.3.0.0
=======

- Refactored array internals around `Data.Vector.Generic`.
- Added `Harpie.Array.Generic`, `Harpie.Array.Storable`, and
  `Harpie.Array.Unboxed`.
- Added `Harpie.Fixed.Generic`, `Harpie.Fixed.Storable`, and
  `Harpie.Fixed.Unboxed`.
- `Harpie.Array` and `Harpie.Fixed` remain as boxed-vector facades.
- Added `fmapA`, `sumA`, `traverseA` to `Harpie.Fixed.Generic` for
  vector-family-generic operations without a `Functor` constraint.
- Updated `Harpie.Hmatrix` to bridge both dynamic and fixed storable
  rank-2 arrays.

0.2.0.0
=======

- Vectorised `Harpie.Shape` value-level API: canonical functions now use
  `Data.Vector.Unboxed (VU.Vector Int)` instead of `[Int]`.
- Added `L`-suffixed list versions of all paired Shape value-level functions
  for backward compatibility.
- Updated `Harpie.Array` internal representation to store shape and strides as
  unboxed vectors; public list-based API preserved.
- Added `indexV` and `tabulateV` for zero-allocation vector-index element access
  and tabulation.
- Rewrote `prod` inner loop to use the vector-native path, restoring
  `mult` performance to the pre-vectorisation baseline.
- Added `Harpie.Hmatrix` bridge module for BLAS fast path on rank-2 arrays.

0.2
===

- removed SomeArray and Arbitrary instances
- removed QuickCheck and quickcheck-instances dependencies

0.1
===

- cleaved from numhask-array-0.11

