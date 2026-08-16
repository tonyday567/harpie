{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

-- | Arrays with shape information and computations at a value-level.
--
-- This is the generic core: the backing vector family is a parameter @v@.
-- Concrete modules ('Harpie.Array', 'Harpie.Array.Storable',
-- 'Harpie.Array.Unboxed') provide type aliases for the common vector families.
module Harpie.Array.Generic
  ( -- * Usage
    -- $usage

    -- * Harpie Arrays
    Array (..),
    array,
    (><),
    validate,
    safeArray,
    unsafeArray,
    unsafeArrayL,
    unsafeModifyShape,
    unsafeModifyVector,

    -- * Dimensions
    Dim,
    Dims,

    -- * Conversion
    asVector,
    arrayV,

    -- * Shape Access
    shape,
    rank,
    size,
    length,
    isNull,

    -- * Indexing
    index,
    (!),
    (!?),
    tabulate,
    backpermute,

    -- * Scalars
    fromScalar,
    toScalar,
    isScalar,
    asSingleton,
    asScalar,

    -- * Array v Creation
    empty,
    range,
    corange,
    indices,
    ident,
    konst,
    singleton,
    diag,
    undiag,

    -- * Element-level functions
    zipWith,
    zipWithSafe,
    modify,
    imap,

    -- * Function generalisers
    rowWise,
    colWise,
    dimsWise,

    -- * Single-dimension functions
    take,
    drop,
    select,
    insert,
    delete,
    append,
    prepend,
    concatenate,
    couple,
    slice,
    rotate,

    -- * Multi-dimension functions
    takes,
    drops,
    indexes,
    slices,
    heads,
    lasts,
    tails,
    inits,

    -- * Function application
    extracts,
    reduces,
    joins,
    joinsSafe,
    join,
    joinSafe,
    traverses,
    maps,
    filters,
    zips,
    zipsSafe,
    modifies,
    diffs,

    -- * Array v expansion & contraction
    expand,
    coexpand,
    contract,
    prod,
    dot,
    mult,
    windows,

    -- * Search
    find,
    findNoOverlap,
    findIndices,
    isPrefixOf,
    isSuffixOf,
    isInfixOf,

    -- * Shape manipulations
    fill,
    cut,
    cutSuffix,
    pad,
    lpad,
    reshape,
    flat,
    repeat,
    cycle,
    rerank,
    reorder,
    squeeze,
    elongate,
    transpose,
    inflate,
    intercalate,
    intersperse,
    concats,
    reverses,
    rotates,

    -- * Sorting
    sorts,
    sortsBy,
    orders,
    ordersBy,

    -- * Transmission
    transmit,
    transmitSafe,
    transmitOp,
    telecasts,
    telecastsSafe,

    -- * Row specializations
    pattern (:<),
    cons,
    uncons,
    pattern (:>),
    snoc,
    unsnoc,

    -- * Shape specializations
    iota,

    -- * Math
    uniform,
  )
where

import Control.Monad hiding (join)
import Data.Bool
import Data.Foldable hiding (find, length, minimum)
import Data.Function
import Data.List qualified as List
import Data.Ord (comparing)
import Data.Vector.Algorithms.Intro (sortBy)
import Data.Vector.Generic qualified as VG
import Data.Vector.Unboxed qualified as VU
import GHC.Generics
import Harpie.Shape hiding (asScalar, asSingleton, concatenate, range, rank, reorder, rerank, rotate, size, squeeze)
import Harpie.Shape qualified as S
import NumHask.Algebra.Additive qualified as Add
import NumHask.Algebra.Multiplicative qualified as Mult
import Prettyprinter hiding (dot, fill)
import System.Random hiding (uniform)
import System.Random.Stateful hiding (uniform)
import Prelude as P hiding (cycle, drop, length, repeat, take, zip, zipWith)

-- $setup
-- >>> :m -Prelude
-- >>> import Prelude hiding (take, drop, zipWith, length, cycle, repeat)
-- >>> import Harpie.Array.Generic
-- >>> import Harpie.Shape qualified as S
-- >>> import Data.Vector (Vector)
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> import Prettyprinter hiding (dot, fill)
-- >>> import Data.List qualified as List
-- >>> let s = 1 :: Array Vector Int
-- >>> s
-- UnsafeArray [] [1]
-- >>> pretty s
-- 1
-- >>> let v = range [3] :: Array Vector Int
-- >>> v
-- UnsafeArray [3] [0,1,2]
-- >>> let m = range [2,3] :: Array Vector Int
-- >>> pretty m
-- [[0,1,2],
--  [3,4,5]]
-- >>> let a = range [2,3,4] :: Array Vector Int
-- >>> a
-- UnsafeArray [2,3,4] [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]
-- >>> pretty a
-- [[[0,1,2,3],
--   [4,5,6,7],
--   [8,9,10,11]],
--  [[12,13,14,15],
--   [16,17,18,19],
--   [20,21,22,23]]]

-- $usage
--
-- Several names used in @harpie@ conflict with [Prelude](https://hackage.haskell.org/package/base/docs/Prelude.html):
--
-- >>> import Prelude hiding (cycle, repeat, take, drop, zipWith, length)
--
-- In general, 'Array' functionality is contained in @Harpie.Array@ and shape  functionality is contained in @Harpie.Shape@. These two modules also have name clashes and at least one needs to be qualified:
--
-- >>> import Harpie.Array as A
-- >>> import Harpie.Shape qualified as S
--
-- [@prettyprinter@](https://hackage.haskell.org/package/prettyprinter) is used to prettily render arrays to better visualise shape.
--
-- >>> import Prettyprinter hiding (dot,fill)
--
-- Examples of arrays:
--
-- An array with no dimensions (a scalar).
--
-- >>> s = 1 :: Array Vector Int
-- >>> s
-- UnsafeArray [] [1]
-- >>> shape s
-- []
-- >>> pretty s
-- 1
--
-- A single-dimension array (a vector).
--
-- >>> let v = range [3]
-- >>> pretty v
-- [0,1,2]
--
-- A two-dimensional array (a matrix).
--
-- >>> let m = range [2,3]
-- >>> pretty m
-- [[0,1,2],
--  [3,4,5]]
--
-- An n-dimensional array (n should be finite).
--
-- >>> a = range [2,3,4]
-- >>> a
-- UnsafeArray [2,3,4] [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]
-- >>> pretty a
-- [[[0,1,2,3],
--   [4,5,6,7],
--   [8,9,10,11]],
--  [[12,13,14,15],
--   [16,17,18,19],
--   [20,21,22,23]]]

-- | A hyperrectangular (or multidimensional) array with a value-level shape.
--
-- >>> let a = array [2,3,4] [1..24] :: Array Vector Int
-- >>> a
-- UnsafeArray [2,3,4] [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24]
--
-- >>> pretty a
-- [[[1,2,3,4],
--   [5,6,7,8],
--   [9,10,11,12]],
--  [[13,14,15,16],
--   [17,18,19,20],
--   [21,22,23,24]]]
data Array v a = UnsafeArray !(VU.Vector Int) !(VU.Vector Int) !(v a)
  deriving stock (Generic)

type role Array representational nominal

instance (Eq (v a)) => Eq (Array v a) where
  (UnsafeArray s _ v) == (UnsafeArray s' _ v') = s == s' && v == v'

instance (Ord (v a)) => Ord (Array v a) where
  compare (UnsafeArray s _ v) (UnsafeArray s' _ v') = compare s s' <> compare v v'

instance (Show (v a)) => Show (Array v a) where
  showsPrec p (UnsafeArray s _ v) =
    showParen (p > 10) $
      showString "UnsafeArray " . shows (VU.toList s) . showString " " . shows v

-- | Internal smart constructor: precomputes strides for O(1) zero-allocation indexing.
unsafeArray :: VU.Vector Int -> v a -> Array v a
unsafeArray s v = UnsafeArray s (VU.drop 1 (VU.scanr (*) 1 s)) v
{-# INLINE unsafeArray #-}

-- | Internal smart constructor from a list shape.
unsafeArrayL :: [Int] -> v a -> Array v a
unsafeArrayL s = unsafeArray (VU.fromList s)
{-# INLINE unsafeArrayL #-}

-- | Functor-like map with an explicit vector constraint.
fmapA :: (VG.Vector v a, VG.Vector v b) => (a -> b) -> Array v a -> Array v b
fmapA f = unsafeModifyVector (VG.map f)

-- | Foldable-like fold with an explicit vector constraint.
foldrA :: (VG.Vector v a) => (a -> b -> b) -> b -> Array v a -> b
foldrA f x0 a = VG.foldr f x0 (asVector a)

-- | Convert an array to a list.
toListA :: (VG.Vector v a) => Array v a -> [a]
toListA = VG.toList . asVector

-- | Traversable-like traversal through a list intermediary.
traverseA :: (Applicative f, VG.Vector v a, VG.Vector v b, VG.Vector v (f b)) => (a -> f b) -> Array v a -> f (Array v b)
traverseA f a = unsafeArray (shape a) . VG.fromList <$> sequenceA (VG.toList (VG.map f (asVector a)))

instance (Show a, Show (v a), VG.Vector v a, VG.Vector v (Array v a)) => Pretty (Array v a) where
  pretty a@(UnsafeArray _ _ v) = case rank a of
    0 -> viaShow (VG.head v)
    1 -> viaShow v
    _ ->
      pretty "["
        <> indent
          0
          ( vsep
              ( punctuate comma $
                  pretty
                    <$> toListA (extracts [0] a)
              )
          )
        <> pretty "]"

-- * conversions

instance (Num a, VG.Vector v a) => Num (Array v a) where
  (+) = zipWith (+)
  (-) = zipWith (-)
  (*) = error "multiplication not defined"
  abs = fmapA abs
  signum = fmapA signum
  fromInteger x = toScalar (fromInteger x)

-- | Convert an array to its underlying vector. Drops shape information.
--
-- > asVector . arrayV [size a] . asVector == a
--
-- >>> asVector (range [2,3])
-- [0,1,2,3,4,5]
asVector :: Array v a -> v a
asVector (UnsafeArray _ _ v) = v

-- | Construct an array from a shape and a list of elements.
--
-- >>> array [2,3] [0..5]
-- UnsafeArray [2,3] [0,1,2,3,4,5]
array :: (VG.Vector v a) => [Int] -> [a] -> Array v a
array s xs = unsafeArrayL s (VG.fromList xs)

-- | Construct an array from a shape and an already-correctly-sized vector.
arrayV :: [Int] -> v a -> Array v a
arrayV = unsafeArrayL

-- | Construct an array from a shape and a value without any shape validation.
--
-- >>> array [2,3] [0..5]
-- UnsafeArray [2,3] [0,1,2,3,4,5]
infixl 4 ><

-- | Construct an Array.
--
-- >>> pretty $ [2,3] >< [0..5]
-- [[0,1,2],
--  [3,4,5]]
(><) :: (VG.Vector v a) => [Int] -> [a] -> Array v a
(><) = array

-- | Validate the size and shape of an array.
--
-- >>> validate (array [2,3,4] [1..23] :: Array Vector Int)
-- False
validate :: (VG.Vector v a) => Array v a -> Bool
validate a = size a == VG.length (asVector a)

-- | Construct an Array, checking shape.
--
-- >>> safeArray [2,3,4] [0..23] == Just a
-- True
safeArray :: (VG.Vector v a) => [Int] -> [a] -> Maybe (Array v a)
safeArray s xs =
  bool Nothing (Just a) (validate a)
  where
    a = array s xs

-- | Unsafely modify an array shape.
--
-- >>> unsafeModifyShape (VU.map (+1)) (array [2,3] [0..5])
-- UnsafeArray [3,4] [0,1,2,3,4,5]
unsafeModifyShape :: (VU.Vector Int -> VU.Vector Int) -> Array v a -> Array v a
unsafeModifyShape f (UnsafeArray s _ v) = unsafeArray (f s) v

-- | Unsafely modify an array vector.
--
-- >>> unsafeModifyVector (VG.map (+1)) (array [2,3] [0..5])
-- UnsafeArray [2,3] [1,2,3,4,5,6]
unsafeModifyVector :: (v a -> v' b) -> Array v a -> Array v' b
unsafeModifyVector f (UnsafeArray s _ v) = unsafeArray s (f v)

-- | Representation of an index into a shape (an [Int]). The index is a dimension of the shape.
type Dim = Int

-- | Representation of indexes into a shape (an [Int]). The indexes are dimensions of the shape.
type Dims = [Int]

-- | shape of an Array
--
-- >>> shape a
-- [2,3,4]
shape :: Array v a -> VU.Vector Int
shape (UnsafeArray s _ _) = s

-- | rank of an Array
--
-- >>> rank a
-- 3
rank :: Array v a -> Int
rank = S.rank . shape

-- | size of an Array, which is the total number of elements, if the Array Vector is valid.
--
-- >>> size a
-- 24
size :: Array v a -> Int
size = S.size . shape

-- | Number of rows (first dimension size) in an Array. As a convention, a scalar value is still a single row.
--
-- >>> length a
-- 2
-- >>> length (toScalar 0)
-- 1
length :: Array v a -> Int
length a = bool (VU.head (shape a)) 1 (VU.null (shape a))

-- | Is the Array Vector empty (has zero number of elements).
--
-- >>> isNull ([2,0] >< [] :: Array Vector ())
-- True
-- >>> isNull ([] >< [4] :: Array Vector Int)
-- False
isNull :: Array v a -> Bool
isNull = (0 ==) . size

-- | Extract an element at an index, unsafely.
--
-- >>> index a [1,2,3]
-- 23
indexV :: (VG.Vector v a) => Array v a -> VU.Vector Int -> a
indexV (UnsafeArray _ strides v) i = VG.unsafeIndex v (S.flattenStrides strides i)
{-# NOINLINE indexV #-}

-- | Extract an element at an index, unsafely.
--
-- >>> index a [1,2,3]
-- 23
index :: (VG.Vector v a) => Array v a -> [Int] -> a
index a = indexV a . VU.fromList
{-# INLINE index #-}

infixl 9 !

-- | Extract an element at an index, unsafely.
--
-- >>> a ! [1,2,3]
-- 23
(!) :: (VG.Vector v a) => Array v a -> [Int] -> a
(!) = index

-- | Extract an element at an index, safely.
--
-- >>> a !? [1,2,3]
-- Just 23
-- >>> a !? [2,3,1]
-- Nothing
(!?) :: (VG.Vector v a) => Array v a -> [Int] -> Maybe a
(!?) a xs = bool Nothing (Just (a ! xs)) (VU.fromList xs `S.isFins` shape a)

-- | Tabulate an array supplying a shape and a vector tabulation function.
tabulateV :: (VG.Vector v a) => VU.Vector Int -> (VU.Vector Int -> a) -> Array v a
tabulateV ds f =
  let strs = S.stridesOf ds
   in UnsafeArray ds strs (VG.generate (S.size ds) (f . S.shapenStrides strs))

-- | Tabulate an array supplying a shape and a tabulation function.
--
-- >>> tabulate [2,3,4] (S.flatten (VU.fromList [2,3,4]) . VU.fromList) == a
-- True
tabulate :: (VG.Vector v a) => [Int] -> ([Int] -> a) -> Array v a
tabulate ds f = tabulateV (VU.fromList ds) (f . VU.toList)

-- | @backpermute@ is a tabulation where the contents of an array do not need to be accessed, and is thus a fulcrum for leveraging laziness and fusion via the rule:
--
-- > backpermute f g (backpermute f' g' a) == backpermute (f . f') (g . g') a
--
-- Many functions in this module are examples of backpermute usage.
--
-- >>> pretty $ backpermute VU.reverse VU.reverse a
-- [[[0,12],
--   [4,16],
--   [8,20]],
--  [[1,13],
--   [5,17],
--   [9,21]],
--  [[2,14],
--   [6,18],
--   [10,22]],
--  [[3,15],
--   [7,19],
--   [11,23]]]
backpermute :: (VG.Vector v a) => (VU.Vector Int -> VU.Vector Int) -> (VU.Vector Int -> VU.Vector Int) -> Array v a -> Array v a
backpermute f g a = tabulateV (f (shape a)) (indexV a . g)
{-# INLINEABLE backpermute #-}

{- RULES
   "backpermute/backpermute" forall f f' g g' (a :: forall a. Array v a)). backpermute f g (backpermute f' g' a) == backpermute (f . f') (g . g') a

-}

-- | Unwrap a scalar.
--
-- >>> let s = array [] [3] :: Array Vector Int
-- >>> fromScalar s
-- 3
fromScalar :: (VG.Vector v a) => Array v a -> a
fromScalar a = index a ([] :: [Int])

-- | Wrap a scalar.
--
-- >>> :t toScalar 2
-- toScalar 2 :: Num a => Array Vector a
toScalar :: (VG.Vector v a) => a -> Array v a
toScalar a = tabulate [] (const a)

-- | Is an array a Scalar?
--
-- >>> isScalar (toScalar (2::Int))
-- True
isScalar :: Array v a -> Bool
isScalar a = rank a == 0

-- | Convert a scalar to being a dimensioned array. Do nothing if not a scalar.
--
-- >>> asSingleton (toScalar 4)
-- UnsafeArray [1] [4]
asSingleton :: Array v a -> Array v a
asSingleton = unsafeModifyShape S.asSingleton

-- | Convert an array with shape [1] to being a scalar (Do nothing if not a shape [1] array).
--
-- >>> asScalar (singleton 3)
-- UnsafeArray [] [3]
asScalar :: Array v a -> Array v a
asScalar = unsafeModifyShape S.asScalar

-- * Creation

-- | An array with no elements.
--
-- >>> empty
-- UnsafeArray [0] []
empty :: (VG.Vector v a) => Array v a
empty = array [0] []

-- | An enumeration of row-major or [lexicographic](https://en.wikipedia.org/wiki/Lexicographic_order) order.
--
-- >>> pretty $ range [2,3]
-- [[0,1,2],
--  [3,4,5]]
range :: (VG.Vector v Int) => [Int] -> Array v Int
range xs = tabulate xs (S.flatten (VU.fromList xs) . VU.fromList)

-- | An enumeration of col-major or [colexicographic](https://en.wikipedia.org/wiki/Lexicographic_order) order.
--
-- >>> pretty (corange [2,3,4])
-- [[[0,6,12,18],
--   [2,8,14,20],
--   [4,10,16,22]],
--  [[1,7,13,19],
--   [3,9,15,21],
--   [5,11,17,23]]]
corange :: (VG.Vector v Int) => [Int] -> Array v Int
corange xs = tabulate xs (S.flatten (VU.fromList (List.reverse xs)) . VU.fromList . List.reverse)

-- | Indices of an array shape.
--
-- >>> pretty $ indices [3,3]
-- [[[0,0],[0,1],[0,2]],
--  [[1,0],[1,1],[1,2]],
--  [[2,0],[2,1],[2,2]]]
indices :: (VG.Vector v [Int]) => [Int] -> Array v [Int]
indices ds = tabulate ds id

-- | The identity array.
--
-- >>> pretty $ ident [3,3]
-- [[1,0,0],
--  [0,1,0],
--  [0,0,1]]
ident :: (Add.Additive a, Mult.Multiplicative a, VG.Vector v a) => [Int] -> Array v a
ident ds = tabulate ds (bool Add.zero Mult.one . isDiag . VU.fromList)

-- | Create an array composed of a single value.
--
-- >>> pretty $ konst [3,2] 1
-- [[1,1],
--  [1,1],
--  [1,1]]
konst :: (VG.Vector v a) => [Int] -> a -> Array v a
konst ds a = tabulate ds (const a)

-- | Create an array of shape [1].
--
-- >>> pretty $ singleton 1
-- [1]
-- >>> singleton 3 == toScalar 3
-- False
--
-- >>> asVector (singleton 3) == asVector (toScalar 3)
-- True
singleton :: (VG.Vector v a) => a -> Array v a
singleton a = unsafeArrayL [1] (VG.singleton a)

-- | Extract the diagonal of an array.
--
-- >>> pretty $ diag (ident [3,3])
-- [1,1,1]
diag ::
  (VG.Vector v a) =>
  Array v a ->
  Array v a
diag a = backpermute S.minDim (VU.replicate (rank a) . S.getDim 0) a

-- | Expand the array to form a diagonal array.
--
-- >>> pretty $ undiag (range [3])
-- [[0,0,0],
--  [0,1,0],
--  [0,0,2]]
undiag ::
  (VG.Vector v a) =>
  (Add.Additive a) =>
  Array v a ->
  Array v a
undiag a = tabulate (VU.toList (shape a VU.++ shape a)) (\xs -> bool Add.zero (index a (List.take (rank a) xs)) (isDiag (VU.fromList xs)))

-- | Zip two arrays at an element level.
--
-- >>> zipWith (-) v v
-- UnsafeArray [3] [0,0,0]
zipWith :: (VG.Vector v a, VG.Vector v b, VG.Vector v c) => (a -> b -> c) -> Array v a -> Array v b -> Array v c
zipWith f (UnsafeArray s _ v) (UnsafeArray _ _ v') = unsafeArray s (VG.zipWith f v v')

-- | Zip two arrays at an element level, checking for shape consistency.
--
-- >>> zipWithSafe (-) (range [3]) (range [4])
-- Nothing
zipWithSafe :: (VG.Vector v a, VG.Vector v b, VG.Vector v c) => (a -> b -> c) -> Array v a -> Array v b -> Maybe (Array v c)
zipWithSafe f (UnsafeArray s _ v) (UnsafeArray s' _ v') = bool Nothing (Just $ unsafeArray s (VG.zipWith f v v')) (s == s')

-- | Modify a single value at an index.
--
-- >>> pretty $ modify [0,0] (const 100) (range [3,2])
-- [[100,1],
--  [2,3],
--  [4,5]]
modify :: (VG.Vector v a) => [Int] -> (a -> a) -> Array v a -> Array v a
modify ds f a = tabulate (VU.toList (shape a)) (\s -> bool id f (s == ds) (index a s))

-- | Maps an index function at element-level.
--
-- >>> pretty $ imap (\xs x -> x - sum xs) a
-- [[[0,0,0,0],
--   [3,3,3,3],
--   [6,6,6,6]],
--  [[11,11,11,11],
--   [14,14,14,14],
--   [17,17,17,17]]]
imap ::
  (VG.Vector v a, VG.Vector v b, VG.Vector v [Int]) =>
  ([Int] -> a -> b) ->
  Array v a ->
  Array v b
imap f a = zipWith f (indices (VU.toList (shape a))) a

-- | With a function that takes dimensions and (type-level) parameters, apply the parameters to the initial dimensions. ie
--
-- > rowWise f xs = f [0..] xs
--
-- >>> rowWise indexes [1,0] a
-- UnsafeArray [4] [12,13,14,15]
rowWise :: (Dims -> [x] -> Array v a -> Array v a) -> [x] -> Array v a -> Array v a
rowWise f xs a = f [0 .. (S.rankL xs - 1)] xs a

-- | With a function that takes dimensions and (type-level) parameters, apply the parameters to the the last dimensions. ie
--
-- > colWise f xs = f (List.reverse [0 .. (rank a - 1)]) xs
--
-- >>> colWise indexes [1,0] a
-- UnsafeArray [2] [1,13]
colWise :: (Dims -> [x] -> Array v a -> Array v a) -> [x] -> Array v a -> Array v a
colWise f xs a = f (List.reverse [(rank a - S.rankL xs) .. (rank a - 1)]) xs a

-- | With a function that takes a dimension and a parameter, fold dimensions and parameters using the function.
--
-- >>> dimsWise take [0,2] [1,2] a
-- UnsafeArray [1,3,2] [0,1,4,5,8,9]
dimsWise :: (Dim -> x -> Array v a -> Array v a) -> Dims -> [x] -> Array v a -> Array v a
dimsWise f ds xs a = foldl' (\a' (d, x) -> f d x a') a (List.zip ds xs)

-- | Take the top-most elements across the specified dimension. Negative values take the bottom-most. No index check is performed.
--
-- > take d x == takes [(d,x)]
--
-- >>> pretty $ take 2 1 a
-- [[[0],
--   [4],
--   [8]],
--  [[12],
--   [16],
--   [20]]]
-- >>> pretty $ take 2 (-1) a
-- [[[3],
--   [7],
--   [11]],
--  [[15],
--   [19],
--   [23]]]
take ::
  (VG.Vector v a) =>
  Dim ->
  Int ->
  Array v a ->
  Array v a
take d t a = backpermute dsNew (modifyDim d (\x -> x + bool 0 (getDim d (shape a) + t) (t < 0))) a
  where
    dsNew = takeDim d (abs t)

-- | Drop the top-most elements across the specified dimension. Negative values take the bottom-most.
--
-- >>> pretty $ drop 2 1 a
-- [[[1,2,3],
--   [5,6,7],
--   [9,10,11]],
--  [[13,14,15],
--   [17,18,19],
--   [21,22,23]]]
-- >>> pretty $ drop 2 (-1) a
-- [[[0,1,2],
--   [4,5,6],
--   [8,9,10]],
--  [[12,13,14],
--   [16,17,18],
--   [20,21,22]]]
drop ::
  (VG.Vector v a) =>
  Dim ->
  Int ->
  Array v a ->
  Array v a
drop d t a = backpermute dsNew (modifyDim d (\x -> x + bool t 0 (t < 0))) a
  where
    dsNew = dropDim d (abs t)

-- | Select an index along a dimension.
--
-- >>> let s = select 2 3 a
-- >>> pretty s
-- [[3,7,11],
--  [15,19,23]]
select ::
  (VG.Vector v a) =>
  Dim ->
  Int ->
  Array v a ->
  Array v a
select d x a = backpermute (deleteDim d) (insertDim d x) a

-- | Insert along a dimension at a position.
--
-- >>> pretty $ insert 2 0 a (konst [2,3] 0)
-- [[[0,0,1,2,3],
--   [0,4,5,6,7],
--   [0,8,9,10,11]],
--  [[0,12,13,14,15],
--   [0,16,17,18,19],
--   [0,20,21,22,23]]]
-- >>> insert 0 0 (toScalar 1) (toScalar 2)
-- UnsafeArray [2] [2,1]
insert ::
  (VG.Vector v a) =>
  Dim ->
  Int ->
  Array v a ->
  Array v a ->
  Array v a
insert d i a b = tabulate (VU.toList (S.incAt d (shape a))) go
  where
    go s =
      let s' = VU.fromList s
       in case compare (S.getDim d s') i of
            EQ -> index b (VU.toList (S.deleteDim d s'))
            LT -> index a s
            GT -> index a (VU.toList (S.decAt d s'))

-- | Delete along a dimension at a position.
--
-- >>> pretty $ delete 2 0 a
-- [[[1,2,3],
--   [5,6,7],
--   [9,10,11]],
--  [[13,14,15],
--   [17,18,19],
--   [21,22,23]]]
delete ::
  (VG.Vector v a) =>
  Dim ->
  Int ->
  Array v a ->
  Array v a
delete d i a = backpermute (decAt d) (\s -> bool (incAt d s) s (getDim d s < i)) a

-- | Insert along a dimension at the end.
--
-- >>> pretty $ append 2 a (konst [2,3] 0)
-- [[[0,1,2,3,0],
--   [4,5,6,7,0],
--   [8,9,10,11,0]],
--  [[12,13,14,15,0],
--   [16,17,18,19,0],
--   [20,21,22,23,0]]]
append ::
  (VG.Vector v a) =>
  Dim ->
  Array v a ->
  Array v a ->
  Array v a
append d a b = insert d (getDim d (shape a)) a b

-- | Insert along a dimension at the beginning.
--
-- >>> pretty $ prepend 2 (konst [2,3] 0) a
-- [[[0,0,1,2,3],
--   [0,4,5,6,7],
--   [0,8,9,10,11]],
--  [[0,12,13,14,15],
--   [0,16,17,18,19],
--   [0,20,21,22,23]]]
prepend ::
  (VG.Vector v a) =>
  Dim ->
  Array v a ->
  Array v a ->
  Array v a
prepend d a b = insert d 0 b a

-- | Concatenate along a dimension.
--
-- >>> shape $ concatenate 1 a a
-- [2,6,4]
-- >>> concatenate 0 (toScalar 1) (toScalar 2)
-- UnsafeArray [2] [1,2]
-- >>> concatenate 0 (toScalar 0) (asArray [1..3])
-- UnsafeArray [4] [0,1,2,3]
concatenate ::
  (VG.Vector v a) =>
  Dim ->
  Array v a ->
  Array v a ->
  Array v a
concatenate d a0 a1 = tabulate (VU.toList (S.concatenate d (shape a0) (shape a1))) go
  where
    go s =
      let s' = VU.fromList s
       in bool
            (index a0 s)
            ( index
                a1
                ( VU.toList
                    ( S.insertDim
                        d
                        (S.getDim d s' - S.getDim d ds0)
                        (S.deleteDim d s')
                    )
                )
            )
            (S.getDim d s' >= S.getDim d ds0)
    ds0 = shape a0

-- | Combine two arrays as a new dimension of a new array.
--
-- >>> pretty $ couple 0 (asArray [1,2,3]) (asArray [4,5,6::Int])
-- [[1,2,3],
--  [4,5,6]]
couple :: (VG.Vector v a) => Int -> Array v a -> Array v a -> Array v a
couple d a a' = concatenate d (elongate d a) (elongate d a')

-- | Slice along a dimension with the supplied offset & length.
--
-- >>> let s = slice 2 1 2 a
-- >>> pretty s
-- [[[1,2],
--   [5,6],
--   [9,10]],
--  [[13,14],
--   [17,18],
--   [21,22]]]
slice ::
  (VG.Vector v a) =>
  Dim ->
  Int ->
  Int ->
  Array v a ->
  Array v a
slice d o l a = backpermute (setDim d l) (modifyDim d (+ o)) a

-- | Rotate an array along a dimension.
--
-- >>> pretty $ rotate 1 2 a
-- [[[8,9,10,11],
--   [0,1,2,3],
--   [4,5,6,7]],
--  [[20,21,22,23],
--   [12,13,14,15],
--   [16,17,18,19]]]
rotate ::
  (VG.Vector v a) =>
  Dim ->
  Int ->
  Array v a ->
  Array v a
rotate d r a = backpermute id (rotateIndex d r (shape a)) a

-- * multi-dimension operators

-- | Takes the top-most elements across the supplied dimension,n tuples. Negative values take the bottom-most.
--
-- > takes == dimsWise take
--
-- >>> pretty $ takes [0,2] [1,-3] a
-- [[[1,2,3],
--   [5,6,7],
--   [9,10,11]]]
takes ::
  (VG.Vector v a) =>
  Dims ->
  [Int] ->
  Array v a ->
  Array v a
takes ds xs a = backpermute (const dsNew) (VU.zipWith (+) start) a
  where
    dsNew = S.setDims (VU.fromList ds) (VU.fromList xsAbs) (shape a)
    start = VU.zipWith (\x s -> bool 0 (s + x) (x < 0)) (S.setDims (VU.fromList ds) (VU.fromList xs) (VU.replicate (rank a) 0)) (shape a)
    xsAbs = fmap abs xs

-- | Drops the top-most elements. Negative values drop the bottom-most.
--
-- >>> pretty $ drops [0,1,2] [1,2,-3] a
-- [[[20]]]
drops ::
  (VG.Vector v a) =>
  Dims ->
  [Int] ->
  Array v a ->
  Array v a
drops ds xs a = backpermute (const dsNew) (VU.zipWith (\d' s' -> bool (d' + s') s' (d' < 0)) xsNew) a
  where
    dsNew = S.dropDims (VU.fromList ds) (VU.fromList xsAbs) (shape a)
    xsNew = S.setDims (VU.fromList ds) (VU.fromList xs) (VU.replicate (rank a) 0)
    xsAbs = fmap abs xs

-- | Select by dimensions and indexes.
--
-- >>> let s = indexes [0,1] [1,1] a
-- >>> pretty s
-- [16,17,18,19]
indexes :: (VG.Vector v a) => Dims -> [Int] -> Array v a -> Array v a
indexes ds xs a = backpermute (const (S.deleteDims (VU.fromList ds) (shape a))) (S.insertDims (VU.fromList ds) (VU.fromList xs)) a

-- | Slice along dimensions with the supplied offsets and lengths.
--
-- >>> let s = slices [2,0] [1,1] [2,1] a
-- >>> pretty s
-- [[[13,14],
--   [17,18],
--   [21,22]]]
slices :: (VG.Vector v a) => Dims -> [Int] -> [Int] -> Array v a -> Array v a
slices ds os ls a = dimsWise (\d (o, l) -> slice d o l) ds (List.zip os ls) a

-- | Select the first element along the supplied dimensions.
--
-- >>> pretty $ heads [0,2] a
-- [0,4,8]
heads :: (VG.Vector v a) => Dims -> Array v a -> Array v a
heads ds a = indexes ds (List.replicate (List.length ds) 0) a

-- | Select the last element along the supplied dimensions.
--
-- >>> pretty $ lasts [0,2] a
-- [15,19,23]
lasts :: (VG.Vector v a) => Dims -> Array v a -> Array v a
lasts ds a = indexes ds lastds a
  where
    lastds = (\i -> S.getDim i (shape a) - 1) <$> ds

-- | Select the tail elements along the supplied dimensions.
--
-- >>> pretty $ tails [0,2] a
-- [[[13,14,15],
--   [17,18,19],
--   [21,22,23]]]
tails :: (VG.Vector v a) => Dims -> Array v a -> Array v a
tails ds a = slices ds os (VU.toList ls) a
  where
    os = List.replicate (List.length ds) 1
    ls = S.getLastPositions (VU.fromList ds) (shape a)

-- | Select the init elements along the supplied dimensions.
--
-- >>> pretty $ inits [0,2] a
-- [[[0,1,2],
--   [4,5,6],
--   [8,9,10]]]
inits :: (VG.Vector v a) => Dims -> Array v a -> Array v a
inits ds a = slices ds os (VU.toList ls) a
  where
    os = List.replicate (List.length ds) 0
    ls = S.getLastPositions (VU.fromList ds) (shape a)

-- | Extracts dimensions to an outer layer.
--
-- >>> pretty $ shape <$> extracts [0] a
-- [[3,4],[3,4]]
extracts ::
  (VG.Vector v (Array v a), VG.Vector v a) =>
  Dims ->
  Array v a ->
  Array v (Array v a)
extracts ds a = tabulate (VU.toList (S.getDims (VU.fromList ds) (shape a))) go
  where
    go s = indexes ds s a

-- | Reduce along specified dimensions, using the supplied fold.
--
-- >>> pretty $ reduces [0] sum a
-- [66,210]
-- >>> pretty $ reduces [0,2] sum a
-- [[12,15,18,21],
--  [48,51,54,57]]
reduces ::
  (VG.Vector v a, VG.Vector v b, VG.Vector v (Array v a)) =>
  Dims ->
  (Array v a -> b) ->
  Array v a ->
  Array v b
reduces ds f a = fmapA f (extracts ds a)

-- | Join inner and outer dimension layers by supplied dimensions. No checks on shape.
--
-- >>> let e = extracts [1,0] a
-- >>> let j = joins [1,0] e
-- >>> a == j
-- True
joins ::
  (VG.Vector v (Array v a), VG.Vector v a) =>
  Dims ->
  Array v (Array v a) ->
  Array v a
joins ds a = tabulate (VU.toList (S.insertDims (VU.fromList ds) so si)) go
  where
    go s = index (index a (VU.toList (S.getDims (VU.fromList ds) (VU.fromList s)))) (VU.toList (S.deleteDims (VU.fromList ds) (VU.fromList s)))
    so = shape a
    si = shape (index a (replicate (rank a) 0))

-- | Join inner and outer dimension layers by supplied dimensions. Check inner layer shape.
--
-- >>> let e = extracts [1,0] a
-- >>> (Just j) = joinsSafe [1,0] e
-- >>> a == j
-- True
joinsSafe ::
  (VG.Vector v (Array v a), VG.Vector v a, VG.Vector v (VU.Vector Int)) =>
  Dims ->
  Array v (Array v a) ->
  Maybe (Array v a)
joinsSafe ds a =
  bool
    Nothing
    (Just $ joins ds a)
    (allEqual (fmapA shape a))

-- | Join inner and outer dimension layers in outer dimension order.
--
-- >>> a == join (extracts [0,1] a)
-- True
join ::
  (VG.Vector v (Array v a), VG.Vector v a) =>
  Array v (Array v a) ->
  Array v a
join a = joins (VU.toList (S.dimsOf (shape a))) a

-- | Join inner and outer dimension layers in outer dimension order, checking for consistent inner dimension shape.
--
-- >>> joinSafe (extracts [0,1] a)
-- Just (UnsafeArray [2,3,4] [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23])
joinSafe ::
  (VG.Vector v (Array v a), VG.Vector v a, VG.Vector v (VU.Vector Int)) =>
  Array v (Array v a) ->
  Maybe (Array v a)
joinSafe a =
  bool
    Nothing
    (Just $ join a)
    (allEqual (fmapA shape a))

-- | Satisfy a predicate across all elements
allEqual :: (Eq a, VG.Vector v a) => Array v a -> Bool
allEqual a = case VG.toList (asVector a) of
  [] -> True
  (x : xs) -> all (== x) xs

-- | Traverse along specified dimensions.
--
-- traverses [1] print (range [2,3])
-- 0
-- 3
-- 1
-- 4
-- 2
-- 5
-- UnsafeArray [2,3] [(),(),(),(),(),()]
traverses ::
  (VG.Vector v a, VG.Vector v b, VG.Vector v (Array v a), VG.Vector v (Array v b), VG.Vector v (f b), VG.Vector v (f (Array v b))) =>
  (Applicative f) =>
  Dims ->
  (a -> f b) ->
  Array v a ->
  f (Array v b)
traverses ds f a = joins ds <$> traverseA (traverseA f) (extracts ds a)

-- | Maps a function along specified dimensions.
--
-- >>> pretty $ maps [1] transpose a
-- [[[0,12],
--   [4,16],
--   [8,20]],
--  [[1,13],
--   [5,17],
--   [9,21]],
--  [[2,14],
--   [6,18],
--   [10,22]],
--  [[3,15],
--   [7,19],
--   [11,23]]]
maps ::
  (VG.Vector v a, VG.Vector v b, VG.Vector v (Array v a), VG.Vector v (Array v b)) =>
  Dims ->
  (Array v a -> Array v b) ->
  Array v a ->
  Array v b
maps ds f a = joins ds (fmapA f (extracts ds a))

-- | Filters along specified dimensions (which are flattened).
--
-- >>> pretty $ filters [0,1] (any ((==0) . (`mod` 7))) a
-- [[0,1,2,3],
--  [4,5,6,7],
--  [12,13,14,15],
--  [20,21,22,23]]
filters ::
  (VG.Vector v a, VG.Vector v (Array v a)) =>
  Dims ->
  (Array v a -> Bool) ->
  Array v a ->
  Array v a
filters ds p a = join (unsafeArrayL [VG.length v'] v')
  where
    v' = VG.filter p (asVector (extracts ds a))

-- | Zips two arrays with a function along specified dimensions.
--
-- >>> pretty $ zips [0,1] (zipWith (,)) a (reverses [0] a)
-- [[[(0,12),(1,13),(2,14),(3,15)],
--   [(4,16),(5,17),(6,18),(7,19)],
--   [(8,20),(9,21),(10,22),(11,23)]],
--  [[(12,0),(13,1),(14,2),(15,3)],
--   [(16,4),(17,5),(18,6),(19,7)],
--   [(20,8),(21,9),(22,10),(23,11)]]]
zips ::
  (VG.Vector v a, VG.Vector v b, VG.Vector v c, VG.Vector v (Array v a), VG.Vector v (Array v b), VG.Vector v (Array v c)) =>
  Dims ->
  (Array v a -> Array v b -> Array v c) ->
  Array v a ->
  Array v b ->
  Array v c
zips ds f a b = joins ds (zipWith f (extracts ds a) (extracts ds b))

-- | Zips two arrays with a function along specified dimensions, checking shapes.
--
-- >>> zipsSafe [0] (zipWith (,)) (asArray [1::Int]) (asArray [1,2::Int])
-- Nothing
zipsSafe ::
  (VG.Vector v a, VG.Vector v b, VG.Vector v c, VG.Vector v (Array v a), VG.Vector v (Array v b), VG.Vector v (Array v c)) =>
  Dims ->
  (Array v a -> Array v b -> Array v c) ->
  Array v a ->
  Array v b ->
  Maybe (Array v c)
zipsSafe ds f a b =
  bool
    (Just $ joins ds (zipWith f (extracts ds a) (extracts ds b)))
    Nothing
    (shape a /= shape b)

-- | Modify using the supplied function along dimensions & positions.
--
-- >>> pretty $ modifies (fmapA (100+)) [2] [0] a
-- [[[100,1,2,3],
--   [104,5,6,7],
--   [108,9,10,11]],
--  [[112,13,14,15],
--   [116,17,18,19],
--   [120,21,22,23]]]
modifies ::
  (VG.Vector v a, VG.Vector v (Array v a)) =>
  (Array v a -> Array v a) ->
  Dims ->
  [Int] ->
  Array v a ->
  Array v a
modifies f ds ps a = joins ds $ modify ps f (extracts ds a)

-- | Apply a binary function between successive slices, across dimensions and lags.
--
-- >>> pretty $ diffs [1] [1] (zipWith (-)) a
-- [[[4,4,4,4],
--   [4,4,4,4]],
--  [[4,4,4,4],
--   [4,4,4,4]]]
diffs :: (VG.Vector v a, VG.Vector v b, VG.Vector v (Array v a), VG.Vector v (Array v b)) => Dims -> [Int] -> (Array v a -> Array v a -> Array v b) -> Array v a -> Array v b
diffs ds xs f a = zips ds f (drops ds xs a) (drops ds (fmap P.negate xs) a)

-- | Product two arrays using the supplied binary function.
--
-- For context, if the function is multiply, and the arrays are tensors,
-- then this can be interpreted as a [tensor product](https://en.wikipedia.org/wiki/Tensor_product).
-- The concept of a tensor product is a dense crossroad, and a complete treatment is elsewhere.  To quote the wiki article:
--
-- ... the tensor product can be extended to other categories of mathematical objects in addition to vector spaces, such as to matrices, tensors, algebras, topological vector spaces, and modules. In each such case the tensor product is characterized by a similar universal property: it is the freest bilinear operation. The general concept of a "tensor product" is captured by monoidal categories; that is, the class of all things that have a tensor product is a monoidal category.
--
-- >>> x = array [3] [1,2,3]
-- >>> pretty $ expand (*) x x
-- [[1,2,3],
--  [2,4,6],
--  [3,6,9]]
--
-- Alternatively, expand can be understood as representing the permutation of element pairs of two arrays, so like the Applicative List instance.
--
-- >>> i2 = indices [2,2]
-- >>> pretty $ expand (,) i2 i2
-- [[[[([0,0],[0,0]),([0,0],[0,1])],
--    [([0,0],[1,0]),([0,0],[1,1])]],
--   [[([0,1],[0,0]),([0,1],[0,1])],
--    [([0,1],[1,0]),([0,1],[1,1])]]],
--  [[[([1,0],[0,0]),([1,0],[0,1])],
--    [([1,0],[1,0]),([1,0],[1,1])]],
--   [[([1,1],[0,0]),([1,1],[0,1])],
--    [([1,1],[1,0]),([1,1],[1,1])]]]]
expand ::
  (VG.Vector v a, VG.Vector v b, VG.Vector v c) =>
  (a -> b -> c) ->
  Array v a ->
  Array v b ->
  Array v c
expand f a b = tabulate (VU.toList (shape a <> shape b)) (\i -> f (index a (List.take r i)) (index b (List.drop r i)))
  where
    r = rank a

-- | Like expand, but permutes the first array first, rather than the second.
--
-- >>> pretty $ expand (,) v (fmapA (+3) v)
-- [[(0,3),(0,4),(0,5)],
--  [(1,3),(1,4),(1,5)],
--  [(2,3),(2,4),(2,5)]]
--
-- >>> pretty $ coexpand (,) v (fmapA (+3) v)
-- [[(0,3),(1,3),(2,3)],
--  [(0,4),(1,4),(2,4)],
--  [(0,5),(1,5),(2,5)]]
--
-- The output shape is @shape b ++ shape a@, so the first array's axes occupy
-- the suffix of the product shape rather than the prefix.  Equivalently, it is
-- @expand@ followed by the block-swap permutation that exchanges the two
-- operand shapes.
coexpand ::
  (VG.Vector v a, VG.Vector v b, VG.Vector v c) =>
  (a -> b -> c) ->
  Array v a ->
  Array v b ->
  Array v c
coexpand f a b = tabulate (VU.toList (shape b <> shape a)) (\i -> f (index a (List.drop rb i)) (index b (List.take rb i)))
  where
    rb = rank b

-- | Contract an array by applying the supplied (folding) function on diagonal elements of the dimensions.
--
-- This generalises a tensor contraction by allowing the number of contracting diagonals to be other than 2.
--
--
-- >>> pretty $ contract [1,2] sum (expand (*) m (transpose m))
-- [[5,14],
--  [14,50]]
contract ::
  (VG.Vector v a, VG.Vector v b, VG.Vector v (Array v a)) =>
  Dims ->
  (Array v a -> b) ->
  Array v a ->
  Array v b
contract ds f a = fmapA (f . diag) (extracts (VU.toList (S.exceptDims (VU.fromList ds) (shape a))) a)

-- | Product two arrays using the supplied function and then contract the result using the supplied matching dimensions and function.
--
-- >>> pretty $ prod [1] [0] sum (*) (range [2,3]) (range [3,2])
-- [[10,13],
--  [28,40]]
--
-- With full laziness, this computation would be equivalent to:
--
-- > f . diag <$> extracts ds' (expand g a b)
prod ::
  (VG.Vector v a, VG.Vector v b, VG.Vector v c, VG.Vector v d) =>
  Dims ->
  Dims ->
  (Array v c -> d) ->
  (a -> b -> c) ->
  Array v a ->
  Array v b ->
  Array v d
prod ds0 ds1 g f a b =
  tabulateV
    (S.deleteDims ds0V (shape a) <> S.deleteDims ds1V (shape b))
    ( \so ->
        g $
          tabulateV
            (S.getDims ds0V (shape a))
            ( \si ->
                f
                  (indexV a (S.insertDims ds0V si (VU.take sp so)))
                  (indexV b (S.insertDims ds1V si (VU.drop sp so)))
            )
    )
  where
    ds0V = VU.fromList ds0
    ds1V = VU.fromList ds1
    sp = rank a - VU.length ds0V

-- | A generalisation of a dot operation, which is a multiplicative expansion of two arrays and sum contraction along the middle two dimensions.
--
-- matrix multiplication
--
-- >>> pretty $ dot sum (*) m (transpose m)
-- [[5,14],
--  [14,50]]
--
-- inner product
--
-- >>> pretty $ dot sum (*) v v
-- 5
--
-- matrix-vector multiplication
-- Note that an Array Vector with shape [3] is neither a row vector nor column vector.
--
-- >>> pretty $ dot sum (*) v (transpose m)
-- [5,14]
--
-- >>> pretty $ dot sum (*) m v
-- [5,14]
dot ::
  (VG.Vector v a, VG.Vector v b, VG.Vector v c, VG.Vector v d, VG.Vector v (Array v c)) =>
  (Array v c -> d) ->
  (a -> b -> c) ->
  Array v a ->
  Array v b ->
  Array v d
dot f g a b = contract [r - 1, r] f (expand g a b)
  where
    r = rank a

-- | Array Vector multiplication.
--
-- matrix multiplication
--
-- >>> pretty $ mult m (transpose m)
-- [[5,14],
--  [14,50]]
--
-- inner product
--
-- >>> pretty $ mult v v
-- 5
--
-- matrix-vector multiplication
--
-- >>> pretty $ mult v (transpose m)
-- [5,14]
--
-- >>> pretty $ mult m v
-- [5,14]
mult ::
  (VG.Vector v a) =>
  (Add.Additive a, Mult.Multiplicative a) =>
  Array v a ->
  Array v a ->
  Array v a
mult a b = prod [rank a - 1] [0] (foldrA (Add.+) Add.zero) (Mult.*) a b

-- | @windows xs@ are xs-sized windows of an array
--
-- >>> shape $ windows [2,2] (range [4,3,2])
-- [3,2,2,2,2]
windows :: (VG.Vector v a) => [Int] -> Array v a -> Array v a
windows xs a = backpermute (S.expandWindows (VU.fromList xs)) (S.indexWindows (S.rankL xs)) a

-- | Find the starting positions of occurences of one array in another.
--
-- >>> a = cycle [4,4] (range [3]) :: Array Vector Int
-- >>> i = array [2,2] [1,2,2,0] :: Array Vector Int
-- >>> pretty $ find i a
-- [[False,True,False],
--  [True,False,False],
--  [False,False,True]]
find :: (Eq (v a), VG.Vector v Bool, VG.Vector v a, VG.Vector v (Array v a)) => Array v a -> Array v a -> Array v Bool
find i a = xs
  where
    i' = rerank (rank a) i
    ws = windows (VU.toList (shape i')) a
    xs = fmapA (== i') (extracts (VU.toList (S.dimWindows (S.expandWindows (shape i') (shape a)) (shape a))) ws)

-- | Find the ending positions of one array in another except where the array overlaps with another copy.
--
-- >>> a = konst [5,5] 1 :: Array Vector Int
-- >>> i = konst [2,2] 1 :: Array Vector Int
-- >>> pretty $ findNoOverlap i a
-- [[True,False,True,False],
--  [False,False,False,False],
--  [True,False,True,False],
--  [False,False,False,False]]
findNoOverlap :: (Eq (v a), VG.Vector v Bool, VG.Vector v a, VG.Vector v (Array v a)) => Array v a -> Array v a -> Array v Bool
findNoOverlap i a = r
  where
    f = find i a

    cl :: [Int] -> [[Int]]
    cl sh =
      List.filter (P.not . any (> 0) . List.init) $
        List.filter (P.not . all (>= 0)) $
          fmap (List.zipWith (\x x0 -> x - x0 + 1) sh) (traverse (\x -> [0 .. (2 * x - 2)]) sh)
    go r' s = index f s && not (any (index r') (List.filter (\x -> S.isFins (VU.fromList x) (shape f)) $ fmap (List.zipWith (+) s) (cl (VU.toList (shape i)))))
    r = tabulate (VU.toList (shape f)) (go r)

-- | Find the indices of the starting location of one array in another.
--
-- >>> b = cycle [4,4] (range [3]) :: Array Vector Int
-- >>> i = array [2,2] [1,2,2,0] :: Array Vector Int
-- >>> pretty $ findIndices i b
-- [[0,1],[1,0],[2,2]]
findIndices :: (Eq (v a), VG.Vector v [Int], VG.Vector v a, VG.Vector v Bool, VG.Vector v ([Int], Bool), VG.Vector v (Array v a)) => Array v a -> Array v a -> Array v [Int]
findIndices i a = unsafeArrayL [VG.length v'] (VG.map fst v')
  where
    v' = VG.filter snd (asVector (imap (,) b))
    b = find i a

-- | Check if the first array is a prefix of the second
--
-- >>> isPrefixOf (array [2,2] [0,1,4,5]) a
-- True
isPrefixOf :: (Eq (v a), VG.Vector v a) => Array v a -> Array v a -> Bool
isPrefixOf p a = p == cut (VU.toList (shape p)) a

-- | Check if the first array is a suffix of the second
--
-- >>> isSuffixOf (array [2,2] [18,19,22,23]) a
-- True
isSuffixOf :: (Eq (v a), VG.Vector v a) => Array v a -> Array v a -> Bool
isSuffixOf p a = p == cutSuffix (VU.toList (shape p)) a

-- | Check if the first array is an infix of the second
--
-- >>> isInfixOf (array [2,2] [18,19,22,23]) a
-- True
isInfixOf :: (Eq (v a), VG.Vector v a, VG.Vector v Bool, VG.Vector v (Array v a)) => Array v a -> Array v a -> Bool
isInfixOf p a = foldrA (||) False (find p a)

-- * shape manipulation

-- | Fill an array with the supplied value without regard to the original shape or cut the array values to match array size.
--
-- > validate (def x a) == True
--
-- >>> pretty $ fill 0 (array [3] [])
-- [0,0,0]
-- >>> pretty $ fill 0 (array [3] [1..4])
-- [1,2,3]
fill :: (VG.Vector v a) => a -> Array v a -> Array v a
fill x (UnsafeArray s _ v) = unsafeArray s (VG.take (S.size s) (v VG.++ VG.replicate (S.size s - VG.length v) x))

-- | Cut an array to form a new (smaller) shape. Errors if the new shape is larger. The old array is reranked to the rank of the new shape first.
--
-- >>> cut [2] (array [4] [0..3] :: Array Vector Int)
-- UnsafeArray [2] [0,1]
cut ::
  (VG.Vector v a) =>
  [Int] ->
  Array v a ->
  Array v a
cut s' a = bool (error "bad cut") (tabulate s' (index a')) (S.isSubset (VU.fromList s') (shape a))
  where
    a' = rerank (List.length s') a

-- | Cut an array to form a new (smaller) shape, using suffix elements. Errors if the new shape is larger. The old array is reranked to the rank of the new shape first.
--
-- >>> cutSuffix [2,2] a
-- UnsafeArray [2,2] [18,19,22,23]
cutSuffix ::
  (VG.Vector v a) =>
  [Int] ->
  Array v a ->
  Array v a
cutSuffix s' a = bool (error "bad cut") (tabulate s' (index a' . List.zipWith (+) diffDim)) (S.isSubset (VU.fromList s') (shape a))
  where
    a' = rerank (List.length s') a
    diffDim = VU.toList (VU.zipWith (-) (shape a') (VU.fromList s'))

-- | Pad an array to form a new shape, supplying a default value for elements outside the shape of the old array. The old array is reranked to the rank of the new shape first.
--
-- >>> pad 0 [5] (array [4] [0..3] :: Array Vector Int)
-- UnsafeArray [5] [0,1,2,3,0]
pad ::
  (VG.Vector v a) =>
  a ->
  [Int] ->
  Array v a ->
  Array v a
pad d s' a = tabulate s' (\s -> bool d (index a' s) (VU.fromList s `S.isFins` shape a'))
  where
    a' = rerank (List.length s') a

-- | Left pad an array to form a new shape, supplying a default value for elements outside the shape of the old array.
--
-- >>> lpad 0 [5] (array [4] [0..3] :: Array Vector Int)
-- UnsafeArray [5] [0,0,1,2,3]
-- >>> pretty $ lpad 0 [3,3] (range [2,2] :: Array Vector Int)
-- [[0,0,0],
--  [0,0,1],
--  [0,2,3]]
lpad ::
  (VG.Vector v a) =>
  a ->
  [Int] ->
  Array v a ->
  Array v a
lpad d s' a = tabulate s' (\s -> bool d (index a' (olds s)) (VU.fromList (olds s) `S.isFins` shape a'))
  where
    a' = rerank (List.length s') a
    gap = VU.toList (VU.zipWith (-) (VU.fromList s') (shape a'))
    olds s = List.zipWith (-) s gap

-- | Reshape an array (with the same or less number of elements).
--
-- >>> pretty $ reshape [4,3,2] a
-- [[[0,1],
--   [2,3],
--   [4,5]],
--  [[6,7],
--   [8,9],
--   [10,11]],
--  [[12,13],
--   [14,15],
--   [16,17]],
--  [[18,19],
--   [20,21],
--   [22,23]]]
reshape ::
  (VG.Vector v a) =>
  [Int] ->
  Array v a ->
  Array v a
reshape s a = backpermute (const (VU.fromList s)) (S.shapen (shape a) . S.flatten (VU.fromList s)) a

-- | Make an Array Vector single dimensional.
--
-- >>> pretty $ flat (range [2,2])
-- [0,1,2,3]
-- >>> pretty (flat $ toScalar 0)
-- [0]
flat :: Array v a -> Array v a
flat a = unsafeModifyShape (VU.singleton . S.size) a

-- | Reshape an array, repeating the original array. The shape of the array should be a suffix of the new shape.
--
-- >>> pretty $ repeat [2,2,2] (array [2] [1,2])
-- [[[1,2],
--   [1,2]],
--  [[1,2],
--   [1,2]]]
--
-- > repeat ds (toScalar x) == konst ds x
repeat ::
  (VG.Vector v a) =>
  [Int] ->
  Array v a ->
  Array v a
repeat s a = backpermute (const (VU.fromList s)) (VU.drop (S.rankL s - rank a)) a

-- | Reshape an array, cycling through the elements without regard to the original shape.
--
-- >>> pretty $ cycle [2,2,2] (array [3] [1,2,3])
-- [[[1,2],
--   [3,1]],
--  [[2,3],
--   [1,2]]]
cycle ::
  (VG.Vector v a) =>
  [Int] ->
  Array v a ->
  Array v a
cycle s a = backpermute (const (VU.fromList s)) (S.shapen (shape a) . (`mod` size a) . S.flatten (VU.fromList s)) a

-- | Change rank by adding new dimensions at the front, if the new rank is greater, or combining dimensions (from left to right) into rows, if the new rank is lower.
--
-- >>> shape (rerank 4 a)
-- [1,2,3,4]
-- >>> shape (rerank 2 a)
-- [6,4]
--
-- > flat == rerank 1
rerank :: Int -> Array v a -> Array v a
rerank r a = unsafeModifyShape (S.rerank r) a

-- | Change the order of dimensions.
--
-- >>> pretty $ reorder [2,0,1] a
-- [[[0,4,8],
--   [12,16,20]],
--  [[1,5,9],
--   [13,17,21]],
--  [[2,6,10],
--   [14,18,22]],
--  [[3,7,11],
--   [15,19,23]]]
reorder ::
  (VG.Vector v a) =>
  Dims ->
  Array v a ->
  Array v a
reorder ds a = backpermute (`S.reorder` VU.fromList ds) (\s -> S.insertDims (VU.fromList ds) s VU.empty) a

-- | Remove single dimensions.
--
-- >>> let sq = array [2,1,3,4,1] [1..24] :: Array Vector Int
-- >>> shape $ squeeze sq
-- [2,3,4]
--
-- >>> shape $ squeeze (singleton 0)
-- []
squeeze ::
  Array v a ->
  Array v a
squeeze a = unsafeModifyShape S.squeeze a

-- | Insert a single dimension at the supplied position.
--
-- >>> shape $ elongate 1 a
-- [2,1,3,4]
-- >>> elongate 0 (toScalar 1)
-- UnsafeArray [1] [1]
elongate ::
  Dim ->
  Array v a ->
  Array v a
elongate d a = unsafeModifyShape (insertDim d 1) a

-- | Reverse indices eg transposes the element A/ijk/ to A/kji/.
--
-- >>> index (transpose a) [1,0,0] == index a [0,0,1]
-- True
-- >>> pretty $ transpose (array [2,2,2] [1..8])
-- [[[1,5],
--   [3,7]],
--  [[2,6],
--   [4,8]]]
transpose :: (VG.Vector v a) => Array v a -> Array v a
transpose a = backpermute VU.reverse VU.reverse a

-- | Inflate an array by inserting a new dimension given a supplied dimension and size.
--
-- alt name: replicate
--
-- >>> pretty $ inflate 0 2 (array [3] [0,1,2])
-- [[0,1,2],
--  [0,1,2]]
inflate ::
  (VG.Vector v a) =>
  Dim ->
  Int ->
  Array v a ->
  Array v a
inflate d n a = backpermute (insertDim d n) (deleteDim d) a

-- | Intercalate an array along dimensions.
--
-- >>> pretty $ intercalate 2 (konst [2,3] 0) a
-- [[[0,0,1,0,2,0,3],
--   [4,0,5,0,6,0,7],
--   [8,0,9,0,10,0,11]],
--  [[12,0,13,0,14,0,15],
--   [16,0,17,0,18,0,19],
--   [20,0,21,0,22,0,23]]]
intercalate :: (VG.Vector v a, VG.Vector v (Array v a)) => Dim -> Array v a -> Array v a -> Array v a
intercalate d i a = joins [d] $ unsafeArrayL [List.length xs] (VG.fromList xs)
  where
    xs = List.intersperse i (VG.toList (asVector (extracts [d] a)))

-- | Intersperse an element along dimensions.
--
-- >>> pretty $ intersperse 2 0 a
-- [[[0,0,1,0,2,0,3],
--   [4,0,5,0,6,0,7],
--   [8,0,9,0,10,0,11]],
--  [[12,0,13,0,14,0,15],
--   [16,0,17,0,18,0,19],
--   [20,0,21,0,22,0,23]]]
intersperse :: (VG.Vector v a, VG.Vector v (Array v a)) => Dim -> a -> Array v a -> Array v a
intersperse d i a = intercalate d (konst (VU.toList (S.deleteDim d (shape a))) i) a

-- | Concatenate and replace dimensions, creating a new dimension at the supplied postion.
--
-- >>> pretty $ concats [0,1] 1 a
-- [[0,4,8,12,16,20],
--  [1,5,9,13,17,21],
--  [2,6,10,14,18,22],
--  [3,7,11,15,19,23]]
concats ::
  (VG.Vector v a) =>
  Dims ->
  Int ->
  Array v a ->
  Array v a
concats ds n a = backpermute (S.concatDims (VU.fromList ds) n) (S.unconcatDimsIndex (VU.fromList ds) n (shape a)) a

-- | Reverses element order along specified dimensions.
--
-- >>> pretty $ reverses [0,1] a
-- [[[20,21,22,23],
--   [16,17,18,19],
--   [12,13,14,15]],
--  [[8,9,10,11],
--   [4,5,6,7],
--   [0,1,2,3]]]
reverses ::
  (VG.Vector v a) =>
  Dims ->
  Array v a ->
  Array v a
reverses ds a = backpermute id (S.reverseIndex (VU.fromList ds) (shape a)) a

-- | Rotate an array by/along dimensions & offsets.
--
-- >>> pretty $ rotates [1] [2] a
-- [[[8,9,10,11],
--   [0,1,2,3],
--   [4,5,6,7]],
--  [[20,21,22,23],
--   [12,13,14,15],
--   [16,17,18,19]]]
rotates ::
  (VG.Vector v a) =>
  Dims ->
  [Int] ->
  Array v a ->
  Array v a
rotates ds rs a = backpermute id (S.rotatesIndex (VU.fromList ds) (VU.fromList rs) (shape a)) a

-- | Generic vector sort helpers.
sortG :: (Ord a, VG.Vector v a, VG.Vector v Int) => v a -> v a
sortG a = VG.backpermute a (orderG a)

sortByG :: (Ord b, VG.Vector v a, VG.Vector v Int) => (a -> b) -> v a -> v a
sortByG c a = VG.backpermute a (orderByG c a)

orderG :: (Ord a, VG.Vector v a, VG.Vector v Int) => v a -> v Int
orderG a = VG.modify (sortBy comp) init0
  where
    comp = comparing $ VG.unsafeIndex a
    init0 = VG.generate (VG.length a) id

orderByG :: (Ord b, VG.Vector v a, VG.Vector v Int) => (a -> b) -> v a -> v Int
orderByG c a = VG.modify (sortBy comp) init0
  where
    comp = comparing $ c . VG.unsafeIndex a
    init0 = VG.generate (VG.length a) id

-- * sorting

-- | Sort an array along the supplied dimensions.
--
-- >>> sorts [0] (array [2,2] [2,3,1,4])
-- UnsafeArray [2,2] [1,4,2,3]
-- >>> sorts [1] (array [2,2] [2,3,1,4])
-- UnsafeArray [2,2] [2,3,1,4]
-- >>> sorts [0,1] (array [2,2] [2,3,1,4])
-- UnsafeArray [2,2] [1,2,3,4]
sorts :: (Ord (v a), VG.Vector v a, VG.Vector v Int, VG.Vector v (Array v a)) => Dims -> Array v a -> Array v a
sorts ds a = joins ds $ unsafeArrayL [VG.length v'] v'
  where
    v' = sortG (asVector (extracts ds a))

-- | The indices into the array if it were sorted by a comparison function along the dimensions supplied.
--
-- >>> import Data.Ord (Down (..))
-- >>> sortsBy [0] (fmapA Down) (array [2,2] [2,3,1,4])
-- UnsafeArray [2,2] [2,3,1,4]
sortsBy :: (Ord (v b), VG.Vector v a, VG.Vector v Int, VG.Vector v (Array v a)) => Dims -> (Array v a -> Array v b) -> Array v a -> Array v a
sortsBy ds c a = joins ds $ unsafeArrayL [VG.length v'] v'
  where
    v' = sortByG c (asVector (extracts ds a))

-- | The indices into the array if it were sorted along the dimensions supplied.
--
-- >>> orders [0] (array [2,2] [2,3,1,4])
-- UnsafeArray [2] [1,0]
orders :: (Ord (v a), VG.Vector v Int, VG.Vector v a, VG.Vector v (Array v a)) => Dims -> Array v a -> Array v Int
orders ds a = unsafeArrayL [VG.length v'] v'
  where
    v' = orderG (asVector (extracts ds a))

-- | The indices into the array if it were sorted by a comparison function along the dimensions supplied.
--
-- >>> import Data.Ord (Down (..))
-- >>> ordersBy [0] (fmapA Down) (array [2,2] [2,3,1,4])
-- UnsafeArray [2] [0,1]
ordersBy :: (Ord (v b), VG.Vector v Int, VG.Vector v a, VG.Vector v (Array v a)) => Dims -> (Array v a -> Array v b) -> Array v a -> Array v Int
ordersBy ds c a = unsafeArrayL [VG.length v'] v'
  where
    v' = orderByG c (asVector (extracts ds a))

-- * transmission

-- | Apply a binary array function to two arrays with matching shapes across the supplied dimensions. No check on shapes.
--
-- >>> a = array [2,3] [0..5]
-- >>> b = array [3] [0..2]
-- >>> pretty $ telecasts [1] [0] (concatenate 0) a b
-- [[0,1,2],
--  [3,4,5],
--  [0,1,2]]
telecasts :: (VG.Vector v a, VG.Vector v b, VG.Vector v c, VG.Vector v (Array v a), VG.Vector v (Array v b), VG.Vector v (Array v c)) => Dims -> Dims -> (Array v a -> Array v b -> Array v c) -> Array v a -> Array v b -> Array v c
telecasts dsa dsb f a b = zipWith f (extracts dsa a) (extracts dsb b) & joins dsa

-- | Apply a binary array function to two arrays with matching shapes across the supplied dimensions. Checks shape.
--
-- >>> a = array [2,3] [0..5]
-- >>> b = array [1] [1]
-- >>> telecastsSafe [0] [0] (zipWith (+)) a b
-- Nothing
telecastsSafe :: (VG.Vector v a, VG.Vector v b, VG.Vector v c, VG.Vector v (Array v a), VG.Vector v (Array v b), VG.Vector v (Array v c)) => Dims -> Dims -> (Array v a -> Array v b -> Array v c) -> Array v a -> Array v b -> Maybe (Array v c)
telecastsSafe dsa dsb f a b =
  bool
    (Just $ telecasts dsa dsb f a b)
    Nothing
    (shape (extracts dsa a) /= shape (extracts dsb b))

-- | Apply a binary array function to two arrays where the shape of the first array is a prefix of the second array. No checks on shape.
--
-- >>> a = array [2,3] [0..5]
-- >>> pretty $ transmit (zipWith (+)) (toScalar 1) a
-- [[1,2,3],
--  [4,5,6]]
transmit :: (VG.Vector v b, VG.Vector v c, VG.Vector v (Array v b), VG.Vector v (Array v c)) => (Array v a -> Array v b -> Array v c) -> Array v a -> Array v b -> Array v c
transmit f a b = maps ds (f a) b
  where
    ds = [(rank a) .. (rank b - 1)]

-- | Apply a binary array function to two arrays where the shape of the first array is a prefix of the second array. Checks shape.
--
-- >>> a = array [2,3] [0..5]
-- >>> transmitSafe (zipWith (+)) (array [3] [1,2,3]) a
-- Nothing
transmitSafe :: (VG.Vector v b, VG.Vector v c, VG.Vector v (Array v b), VG.Vector v (Array v c)) => (Array v a -> Array v b -> Array v c) -> Array v a -> Array v b -> Maybe (Array v c)
transmitSafe f a b = bool Nothing (Just $ transmit f a b) (VU.toList (shape a) `List.isPrefixOf` VU.toList (shape b))

-- | Transmit an operation if the first array is a prefix of the second or vice versa.
--
-- >>> pretty $ transmitOp (*) a (asArray [1,2])
-- [[[0,1,2,3],
--   [4,5,6,7],
--   [8,9,10,11]],
--  [[24,26,28,30],
--   [32,34,36,38],
--   [40,42,44,46]]]
transmitOp :: (VG.Vector v a, VG.Vector v b, VG.Vector v c, VG.Vector v (Array v a), VG.Vector v (Array v b), VG.Vector v (Array v c)) => (a -> b -> c) -> Array v a -> Array v b -> Array v c
transmitOp f a b
  | shape a == shape b = zipWith f a b
  | VU.toList (shape a) `List.isPrefixOf` VU.toList (shape b) = transmit (zipWith f) a b
  | VU.toList (shape b) `List.isPrefixOf` VU.toList (shape a) = transmit (zipWith (flip f)) b a
  | otherwise = error "bad shapes"

-- | Vector specialisation of 'range'
--
-- >>> iota 5
-- UnsafeArray [5] [0,1,2,3,4]
iota :: (VG.Vector v Int) => Int -> Array v Int
iota n = range [n]

-- * row (first dimension) specializations

-- | Add a new row
--
-- >>> pretty $ cons (array [2] [0,1]) (array [2,2] [2,3,4,5])
-- [[0,1],
--  [2,3],
--  [4,5]]
cons :: (VG.Vector v a) => Array v a -> Array v a -> Array v a
cons = prepend 0

-- | split an array into the first row and the remaining rows.
--
-- >>> uncons (array [3,2] [0..5])
-- (UnsafeArray [2] [0,1],UnsafeArray [2,2] [2,3,4,5])
uncons :: (VG.Vector v a) => Array v a -> (Array v a, Array v a)
uncons a = (heads [0] a', tails [0] a')
  where
    a' = asSingleton a

-- | Convenience pattern for row extraction and consolidation at the beginning of an Array.
--
-- >>> (x:<xs) = array [4] [0..3]
-- >>> x
-- UnsafeArray [] [0]
-- >>> xs
-- UnsafeArray [3] [1,2,3]
-- >>> (x:<xs)
-- UnsafeArray [4] [0,1,2,3]
pattern x :< xs <- (uncons -> (x, xs))
  where
    x :< xs = cons x xs

infix 5 :<

-- | Add a new row at the end
--
-- >>> pretty $ snoc (array [2,2] [0,1,2,3]) (array [2] [4,5])
-- [[0,1],
--  [2,3],
--  [4,5]]
snoc :: (VG.Vector v a) => Array v a -> Array v a -> Array v a
snoc = append 0

-- | split an array into the initial rows and the last row.
--
-- >>> unsnoc (array [3,2] [0..5])
-- (UnsafeArray [2,2] [0,1,2,3],UnsafeArray [2] [4,5])
unsnoc :: (VG.Vector v a) => Array v a -> (Array v a, Array v a)
unsnoc a = (inits [0] a', lasts [0] a')
  where
    a' = asSingleton a

-- | Convenience pattern for row extraction and consolidation at the end of an Array.
--
-- >>> (xs:>x) = array [4] [0..3]
-- >>> x
-- UnsafeArray [] [3]
-- >>> xs
-- UnsafeArray [3] [0,1,2]
-- >>> (xs:>x)
-- UnsafeArray [4] [0,1,2,3]
pattern xs :> x <- (unsnoc -> (xs, x))
  where
    xs :> x = snoc xs x

infix 5 :>

-- * Math

-- | Generate an array of uniform random variates between a range.
--
-- >>> import System.Random.Stateful hiding (uniform)
-- >>> g <- newIOGenM (mkStdGen 42)
-- >>> u <- uniform g [2,3,4] (0,9 :: Int)
-- >>> pretty u
-- [[[0,7,0,2],
--   [1,7,4,2],
--   [5,9,8,2]],
--  [[9,8,1,0],
--   [2,2,8,2],
--   [2,8,0,6]]]
uniform :: (StatefulGen g m, UniformRange a, VG.Vector v a) => g -> [Int] -> (a, a) -> m (Array v a)
uniform g ds r = do
  v <- VG.replicateM (S.size (VU.fromList ds)) (uniformRM r g)
  pure $ unsafeArray (VU.fromList ds) v

