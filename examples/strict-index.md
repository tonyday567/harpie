# Harpie Index Plumbing: List-Allocation Bottleneck

**Severity:** Critical — blocks all LLM-scale linear algebra.

**Status:** Open

## Problem

`flatten` and `shapen` in `Harpie.Shape` allocate cons-cell lists on every
array index access. For `tabulate`/`backpermute` arrays (used by `mult`,
`contract`, `expand`, `transpose`, etc.), this turns every element access
into a list-allocation storm.

### Performance impact (64×64 matrix multiply, all-ones)

```
mult [64,16] [16,64]:
  - 4,096 output elements
  - 256 contract-sum iterations per output element
  - 1,048,576 expand-thunk accesses
  - ~45 list cons cells per access (flatten + shapen × multiple layers)
  - 424 MB total allocation (measured via +RTS -s)
  - 120 ms wall time

Extrapolated to 1024×768 (GPT-2 single-head attention):
  - ~4B expand-thunk accesses
  - ~200 GB allocation
  - minutes per single matmul
```

### Root cause

```haskell
-- Harpie/Shape.hs
flatten :: [Int] -> [Int] -> Int
flatten ns xs = sum $ zipWith (*) xs (drop 1 $ scanr (*) 1 ns)
-- Allocates: scanr list (rank+1 cells), drop list (rank cells),
--            zipWith list (rank cells) = ~3*rank cons cells per call

shapen :: [Int] -> Int -> [Int]
shapen ns x =
  fst $ foldr (\a (acc, r) -> let (d, m) = divMod r a in (m : acc, d)) ([], x) ns
-- Allocates: rank cons cells (the result list) per call
```

These are called on every `V.!` into a `tabulate`/`backpermute` array:

```haskell
-- Harpie/Array.hs
index (UnsafeArray s v) i = V.unsafeIndex v (flatten s i)
--                                          ^^^^^^^ allocates
tabulate ds f = UnsafeArray ds (V.generate (product ds) (f . shapen ds))
--                                                       ^^^^^^ allocates per thunk force
```

### Why it's structural

`tabulate` and `backpermute` are the foundation of harpie's lazy array
machinery. Every `mult`, `contract`, `expand`, `transpose`, `reshape`,
`select`, `take`, `drop` goes through them. The list allocation is
unavoidable with the current `[Int]`-based shape representation.

## Proposed fix

Replace `flatten` and `shapen` with unboxed integer arithmetic that
avoids intermediate lists entirely.

### Step 1: Unboxed flatten

Compute the flat index using a strict accumulating fold over strides
without allocating a list:

```haskell
flattenStrict :: [Int] -> [Int] -> Int
flattenStrict sh idx = go (drop 1 (scanrStrict (*) 1 sh)) idx 0
  where
    go (s:ss) (i:is) !acc = go ss is (acc + i * s)
    go _      _      !acc = acc
```

But this still allocates the stride list. Better: precompute strides
once per array and store them.

### Step 2: Store strides in Array

Change `Array` from:

```haskell
data Array a = UnsafeArray [Int] (V.Vector a)
```

to:

```haskell
data Array a = UnsafeArray { shape_ :: [Int], strides_ :: [Int], data_ :: V.Vector a }
```

Where `strides_` is `drop 1 (scanr (*) 1 shape_)`, precomputed at
construction time. Then:

```haskell
index (UnsafeArray _ strides v) is = V.unsafeIndex v (flatStrict strides is)
  where
    flatStrict (s:ss) (i:is) = i*s + flatStrict ss is
    flatStrict _ _ = 0
```

Zero allocation per index call.

### Step 3: Replace shapen in tabulate

For `tabulate`, replace `shapen` with an unrolled integer version
that produces indices as `Int` components without list allocation.
Alternatively, avoid `shapen` entirely by working with flat indices
directly where possible.

## Workaround

For circuits-llm, use hmatrix (BLAS-backed dense matrices) for the
attention linear algebra. Harpie Arrays remain for BPE and other
non-matmul operations.

## Files to modify

- `src/Harpie/Shape.hs` — `flatten`, `shapen`
- `src/Harpie/Array.hs` — `index` on `tabulate`/`backpermute`, `Array` data type
