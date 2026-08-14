{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Arrays with shape information and computations at a type-level.
module Harpie.Fixed.Generic
  ( -- * Usage
    -- $usage

    -- * Fixed Arrays
    Array (..),
    unsafeArray,
    validate,
    safeArray,
    array,
    unsafeModifyShape,
    unsafeModifyVector,

    -- * Dimensions
    Dim,
    pattern Dim,
    Dims,
    pattern Dims,

    -- * Conversion
    FromVector (..),
    toDynamic,
    with,

    -- * Shape Access
    shape,
    rank,
    size,
    length,
    isNull,

    -- * Indexing
    index,
    unsafeIndex,
    (!),
    (!?),
    tabulate,
    unsafeTabulate,
    backpermute,
    unsafeBackpermute,

    -- * Scalars
    fromScalar,
    toScalar,
    isScalar,
    asSingleton,
    asScalar,

    -- * Array Creation
    empty,
    range,
    corange,
    indices,
    ident,
    konst,
    singleton,
    diag,
    undiag,

    -- * Vector-level helpers
    fmapA,
    sumA,
    traverseA,

    -- * Element-level functions
    zipWith,
    modify,
    imap,

    -- * Function generalisers
    rowWise,
    colWise,

    -- * Single-dimension functions
    take,
    takeB,
    drop,
    dropB,
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
    takeBs,
    drops,
    dropBs,
    indexes,
    indexesT,
    slices,
    heads,
    lasts,
    tails,
    inits,

    -- * Function application
    extracts,
    reduces,
    joins,
    join,
    traverses,
    maps,
    filters,
    zips,
    modifies,
    diffs,

    -- * Array expansion & contraction
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
    telecasts,
    transmit,

    -- * Row specializations
    pattern (:<),
    cons,
    uncons,
    pattern (:>),
    snoc,
    unsnoc,

    -- * Shape specializations
    Vector,
    vector,
    vector',
    iota,
    Matrix,

    -- * Math
    uniform,
  )
where

import Data.Bool
import Data.Foldable hiding (find, length, minimum)
import Data.List qualified as List
import Data.Maybe
import Data.Vector.Generic qualified as VG
import Data.Vector.Unboxed qualified as VU
import Fcf hiding (type (&&), type (+), type (++), type (-))
import Fcf qualified
import Fcf.Data.List
import GHC.Generics
import GHC.TypeNats
import Harpie.Array.Generic qualified as A
import Harpie.Shape hiding (asScalar, asSingleton, concatenate, range, rank, reorder, rerank, rotate, size, squeeze)
import Harpie.Shape qualified as S
import Harpie.Sort
import NumHask.Algebra.Additive qualified as Add
import NumHask.Algebra.Multiplicative qualified as Mult
import Prettyprinter hiding (dot, fill)
import System.Random hiding (uniform)
import System.Random.Stateful hiding (uniform)
import Unsafe.Coerce
import Prelude as P hiding (cycle, drop, length, repeat, sequence, take, zipWith)

-- $setup
--
-- >>> :m -Prelude
-- >>> :set -XDataKinds
-- >>> :set -Wno-type-defaults
-- >>> :set -Wno-name-shadowing
-- >>> import Prelude hiding (cycle, repeat, take, drop, zipWith, length)
-- >>> import Harpie.Fixed as F
-- >>> import Harpie.Shape qualified as S
-- >>> import Harpie.Shape (SNats, Fin (..), Fins (..))
-- >>> import GHC.TypeNats
-- >>> import Data.List qualified as List
-- >>> import Prettyprinter hiding (dot,fill)
-- >>> import Data.Functor.Rep
-- >>> s = 1 :: Array '[] Int
-- >>> s
-- [1]
-- >>> shape s
-- []
-- >>> pretty s
-- 1
-- >>> let v = range @'[3]
-- >>> pretty v
-- [0,1,2]
-- >>> let m = range @[2,3]
-- >>> pretty m
-- [[0,1,2],
--  [3,4,5]]
-- >>> a = range @[2,3,4]
-- >>> a
-- [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]
-- >>> pretty a
-- [[[0,1,2,3],
--   [4,5,6,7],
--   [8,9,10,11]],
--  [[12,13,14,15],
--   [16,17,18,19],
--   [20,21,22,23]]]
-- >>> e = array @[3,3] @Double [4,12,-16,12,37,-43,-16,-43,98]
-- >>> l = chol e

-- $usage
--
-- >>> :set -XDataKinds
--
-- Several names used in @harpie@ conflict with [Prelude](https://hackage.haskell.org/package/base/docs/Prelude.html):
--
-- >>> import Prelude hiding (cycle, repeat, take, drop, zipWith, length)
--
-- In general, 'Array' functionality is contained in @Harpie.Fixed@ and shape  functionality is contained in @Harpie.Shape@. These two modules also have name clashes and at least one needs to be qualified:
--
-- >>> import Harpie.Fixed as F
-- >>> import Harpie.Shape qualified as S
--
-- [@prettyprinter@](https://hackage.haskell.org/package/prettyprinter) is used to prettily render arrays to better visualise shape.
--
-- >>> import Prettyprinter hiding (dot,fill)
--
-- The 'Representable' class from [@adjunctions@](https://hackage.haskell.org/package/adjunctions) is used heavily by the module.
--
-- >>> import Data.Functor.Rep
--
-- An important base accounting of 'Array' shape is the singleton types 'SNat' (a type-level 'Natural' or 'Nat') from [GHC.TypeNats](https://hackage.haskell.org/package/base/docs/GHC-TypeNats.html) in base.
--
-- >>> import GHC.TypeNats
--
-- The [first-class-families](https://hackage.haskell.org/package/first-class-families) library was used to code most of type-level constraint logic.
--
-- >>> import Fcf qualified
--
-- Examples of arrays:
--
-- An array with no dimensions (a scalar).
--
-- >>> s = 1 :: Array '[] Int
-- >>> s
-- [1]
-- >>> shape s
-- []
-- >>> pretty s
-- 1
--
-- A single-dimension array (a vector).
--
-- >>> let v = range @'[3]
-- >>> pretty v
-- [0,1,2]
--
-- A two-dimensional array (a matrix).
--
-- >>> let m = range @[2,3]
-- >>> pretty m
-- [[0,1,2],
--  [3,4,5]]
--
-- An n-dimensional array (n should be finite).
--
-- >>> a = range @[2,3,4]
-- >>> a
-- [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]
-- >>> pretty a
-- [[[0,1,2,3],
--   [4,5,6,7],
--   [8,9,10,11]],
--  [[12,13,14,15],
--   [16,17,18,19],
--   [20,21,22,23]]]
--
-- Conversion to a dynamic, value-level shaped 'Harpie.Array.Array'
--
-- >>> toDynamic a
-- UnsafeArray [2,3,4] [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]

-- | A hyperrectangular (or multidimensional) array with a type-level shape.
--
-- >>> array @[2,3,4] @Int [1..24]
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24]
-- >>> array [1..24] :: Array '[2,3,4] Int
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24]
-- >>> pretty (array @[2,3,4] @Int [1..24])
-- [[[1,2,3,4],
--   [5,6,7,8],
--   [9,10,11,12]],
--  [[13,14,15,16],
--   [17,18,19,20],
--   [21,22,23,24]]]
--
-- >>> array [1,2,3] :: Array '[2,2] Int
-- *** Exception: Shape Mismatch
-- ...
--
-- In many situations, the use of  [TypeApplication](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_applications.html) can lead to a clean coding style.
--
-- >>> array @[2,3] @Int [1..6]
-- [1,2,3,4,5,6]
--
-- The main computational entry and exit points are often via 'index' and 'tabulate' with arrays indexed by 'Fins':
--
-- >>> index a (S.UnsafeFins [1,2,3])
-- 23
--
-- >>> :t tabulate id :: Array [2,3] (Fins [2,3])
-- tabulate id :: Array [2,3] (Fins [2,3])
--   :: Array [2, 3] (Fins [2, 3])
-- >>> pretty (tabulate id :: Array [2,3] (Fins [2,3]))
-- [[[0,0],[0,1],[0,2]],
--  [[1,0],[1,1],[1,2]]]
type role Array nominal nominal nominal

newtype Array v (s :: [Nat]) a = Array (v a)
  deriving stock (Functor, Foldable, Generic, Traversable)
  deriving newtype (Eq, Ord, Show)

instance (Num a, KnownNats s, VG.Vector v a, Functor v) => Num (Array v s a) where
  (+) = zipWith (+)
  (-) = zipWith (-)
  (*) = error "multiplication not defined"
  abs = fmap abs
  signum = fmap signum
  fromInteger x = konst @s (fromInteger x)

instance (KnownNats s, Show a, Show (v a), VG.Vector v a, VG.Vector v (A.Array v a)) => Pretty (Array v s a) where
  pretty = pretty . toDynamic

-- | Tabulate an array from a function on 'Fins'.
tabulate :: forall v s a. (KnownNats s, VG.Vector v a) => (Fins s -> a) -> Array v s a
tabulate f =
  Array $ VG.generate (S.size (VU.fromList s)) (f . UnsafeFins . VU.toList . S.shapen (VU.fromList s))
  where
    s = valuesOf @s
{-# INLINE tabulate #-}

-- | Index into an array using 'Fins'.
index :: forall v s a. (KnownNats s, VG.Vector v a) => Array v s a -> Fins s -> a
index (Array v) i = VG.unsafeIndex v (S.flatten (VU.fromList s) (VU.fromList (fromFins i)))
  where
    s = valuesOf @s
{-# INLINE index #-}

-- | Conversion to and from a `V.Vector`
--
-- Note that conversion of an 'Array' to a vector drops shape information, so that:
--
-- > vectorAs . asVector == id
-- > asVector . vectorAs == 'flat'
--
-- >>> asVector (range @[2,3])
-- [0,1,2,3,4,5]
--
-- >>> -- >>> vectorAs (VG.fromList [0..5]) :: Array [2,3] Int
-- [0,1,2,3,4,5]
class (VG.Vector v a) => FromVector t v a | t -> a where
  asVector :: t -> v a
  vectorAs :: v a -> t

instance {-# INCOHERENT #-} (VG.Vector v a) => FromVector (v a) v a where
  asVector = id
  vectorAs = id

instance {-# INCOHERENT #-} (VG.Vector v a) => FromVector [a] v a where
  asVector = VG.fromList
  vectorAs = VG.toList

instance {-# INCOHERENT #-} (VG.Vector v a) => FromVector (Array v s a) v a where
  asVector (Array v) = v
  vectorAs v = Array v

-- | Construct an array without shape validation.
--
-- >>> unsafeArray [0..4] :: Array [2,3] Int
-- [0,1,2,3,4]
unsafeArray :: (KnownNats s, FromVector t v a, VG.Vector v a) => t -> Array v s a
unsafeArray (asVector -> v) = Array v

-- | Validate the size and shape of an array.
--
-- >>> validate (unsafeArray [0..4] :: Array [2,3] Int)
-- False
validate :: (KnownNats s, VG.Vector v a) => Array v s a -> Bool
validate a@(Array v) = size a == VG.length v

-- | Construct an Array, checking shape.
--
-- >>> (safeArray [0..23] :: Maybe (Array [2,3,4] Int)) == Just a
-- True
safeArray :: (KnownNats s, FromVector t v a, VG.Vector v a) => t -> Maybe (Array v s a)
safeArray v =
  bool Nothing (Just a) (validate a)
  where
    a = unsafeArray v

-- | Construct an Array, throwing an exception on a bad shape.
--
-- >>> array [0..22] :: Array [2,3,4] Int
-- *** Exception: Shape Mismatch
-- ...
array :: forall v s a t. (KnownNats s, FromVector t v a, VG.Vector v a) => t -> Array v s a
array v =
  fromMaybe (error "Shape Mismatch") (safeArray v)

-- | Unsafely modify an array shape.
--
-- >>> pretty (unsafeModifyShape @[3,2] (array @[2,3] @Int [0..5]))
-- [[0,1],
--  [2,3],
--  [4,5]]
unsafeModifyShape :: forall v s' s a. (KnownNats s, KnownNats s', VG.Vector v a) => Array v s a -> Array v s' a
unsafeModifyShape (Array v) = Array v

-- | Unsafely modify an array vector.
--
-- >>> -- >>> pretty (unsafeModifyVector (V.map (+1)) (array [0..5] :: Array [2,3] Int))
-- [[1,2,3],
--  [4,5,6]]
unsafeModifyVector :: forall v s a b. (KnownNats s, VG.Vector v a, VG.Vector v b) => (v a -> v b) -> Array v s a -> Array v s b
unsafeModifyVector f (Array v) = Array (f v)

-- Helpers that operate directly on the underlying vector, avoiding
-- Functor/Foldable/Traversable constraints on the vector type.

fmapA :: forall v s a b. (KnownNats s, VG.Vector v a, VG.Vector v b) => (a -> b) -> Array v s a -> Array v s b
fmapA f = unsafeModifyVector (VG.map f)

foldlA' :: forall v s a b. (VG.Vector v a) => (b -> a -> b) -> b -> Array v s a -> b
foldlA' f z (Array v) = VG.foldl' f z v

toListA :: forall v s a. (VG.Vector v a) => Array v s a -> [a]
toListA (Array v) = VG.toList v

sumA :: forall v s a. (Add.Additive a, VG.Vector v a) => Array v s a -> a
sumA (Array v) = VG.foldr (Add.+) Add.zero v

orA :: forall v s. (VG.Vector v Bool) => Array v s Bool -> Bool
orA (Array v) = VG.or v

traverseA :: forall f v s a b. (Applicative f, KnownNats s, VG.Vector v a, VG.Vector v b, VG.Vector v (f b)) => (a -> f b) -> Array v s a -> f (Array v s b)
traverseA f (Array v) = Array . VG.fromList <$> sequenceA (VG.toList (VG.map f v))

-- | Representation of an index into a shape (a type-level [Nat]). 'Dim @0' is commonly thought of as the row of an array.
type Dim = SNat

-- | Pattern synonym for a 'Dim'
pattern Dim :: () => (KnownNat n) => SNat n
pattern Dim = SNat

{-# COMPLETE Dim #-}

-- | Representation of indexes into a shape (a type-level [Nat]). The indexes are dimensions of the shape.
type Dims = SNats

-- | Pattern synonym for a 'Dims'
pattern Dims :: () => (KnownNats ns) => SNats ns
pattern Dims = SNats

{-# COMPLETE Dims #-}

-- | Convert to a dynamic array with shape at the value level.
--
-- >>> toDynamic a
-- UnsafeArray [2,3,4] [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]
toDynamic :: (KnownNats s, VG.Vector v a) => Array v s a -> A.Array v a
toDynamic a@(Array v) = A.arrayV (VU.toList (shape a)) v

-- | Use a dynamic array in a fixed context.
--
-- >>> import qualified Harpie.Array as A
-- >>> with (A.range [2,3,4]) show
-- "[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]"
--
-- This doesn't work for anything more complex where KnownNats need to be type computed:
--
-- >>> :t with (A.range [2,3,4]) (pretty . F.takes (Dims @'[0]) (S.SNats @'[1]))
-- ...
--     • Could not deduce ‘S.KnownNats (Fcf.Data.List.Drop_ 1 s)’
-- ...
with ::
  forall v a r.
  (VG.Vector v a) =>
  A.Array v a ->
  (forall s. (KnownNats s) => Array v s a -> r) ->
  r
with d f =
  withSomeSNats (fromIntegral <$> VU.toList (A.shape d)) $ \(SNats :: SNats s) -> withKnownNats (SNats @s) (f (array @v @s (A.asVector d)))

-- | Get shape of an Array as a value.
--
-- >>> shape a
-- [2,3,4]
shape :: forall v a s. (KnownNats s, VG.Vector v a) => Array v s a -> VU.Vector Int
shape _ = VU.fromList (valuesOf @s)
{-# INLINE shape #-}

-- | Get rank of an Array as a value.
--
-- >>> rank a
-- 3
rank :: forall v a s. (KnownNats s, VG.Vector v a) => Array v s a -> Int
rank = S.rank . shape
{-# INLINE rank #-}

-- | Get size of an Array as a value.
--
-- >>> size a
-- 24
size :: forall v a s. (KnownNats s, VG.Vector v a) => Array v s a -> Int
size = S.size . shape
{-# INLINE size #-}

-- | Number of rows (first dimension size) in an Array. As a convention, a scalar value is still a single row.
--
-- >>> length a
-- 2
-- >>> length (toScalar 0)
-- 1
length :: (KnownNats s, VG.Vector v a) => Array v s a -> Int
length a = case VU.toList (shape a) of
  [] -> 1
  (x : _) -> x

-- | Is the Array empty (has zero number of elements).
--
-- >>> isNull (array [] :: Array [2,0] ())
-- True
-- >>> isNull (array [4] :: Array '[] Int)
-- False
isNull :: (KnownNats s, VG.Vector v a) => Array v s a -> Bool
isNull = (0 ==) . size

-- | Extract an element at an index, unsafely.
--
-- >>> unsafeIndex a [1,2,3]
-- 23
unsafeIndex :: (KnownNats s, VG.Vector v a) => Array v s a -> [Int] -> a
unsafeIndex a xs = index a (UnsafeFins xs)

-- | Extract an element at an index, unsafely.
--
-- >>> a ! [1,2,3]
-- 23
(!) :: (KnownNats s, VG.Vector v a) => Array v s a -> [Int] -> a
(!) a xs = index a (UnsafeFins xs)

infixl 9 !

-- | Extract an element at an index, safely.
--
-- >>> a !? [1,2,3]
-- Just 23
-- >>> a !? [2,3,1]
-- Nothing
(!?) :: (KnownNats s, VG.Vector v a) => Array v s a -> [Int] -> Maybe a
(!?) a xs = index a <$> safeFins xs

infixl 9 !?

-- | Tabulate unsafely.
--
-- >>> :t tabulate @(Array [2,3]) id
-- tabulate @(Array [2,3]) id :: Array [2, 3] (Fins [2, 3])
-- >>> :t unsafeTabulate @[2,3] id
-- unsafeTabulate @[2,3] id :: Array [2, 3] [Int]
-- >>> pretty $ unsafeTabulate @[2,3] id
-- [[[0,0],[0,1],[0,2]],
--  [[1,0],[1,1],[1,2]]]
unsafeTabulate :: (KnownNats s, VG.Vector v a) => ([Int] -> a) -> Array v s a
unsafeTabulate f = tabulate (f . fromFins)

-- | @backpermute@ is a tabulation where the contents of an array do not need to be accessed, and is thus a fulcrum for leveraging laziness and fusion via the rule:
--
-- > backpermute f (backpermute f' a) == backpermute (f . f') a
--
-- Many functions in this module are examples of backpermute usage.
--
-- >>> pretty $ backpermute @[4,3,2] (UnsafeFins . List.reverse . fromFins) a
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
backpermute :: forall v s' s a. (KnownNats s, KnownNats s', VG.Vector v a) => (Fins s' -> Fins s) -> Array v s a -> Array v s' a
backpermute f a = tabulate (index a . f)
{-# INLINEABLE backpermute #-}

{- RULES
   "backpermute/backpermute" forall f f' (a :: forall a. Array a)). backpermute f (backpermute f' a) == backpermute (f . f') a
-}

-- | Unsafe backpermute
--
-- >>> pretty $ unsafeBackpermute @[4,3,2] List.reverse a
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
unsafeBackpermute :: forall v s' s a. (KnownNats s, KnownNats s', VG.Vector v a) => ([Int] -> [Int]) -> Array v s a -> Array v s' a
unsafeBackpermute f a = tabulate (index a . UnsafeFins . f . fromFins)

{- RULES
   "unsafeBackpermute/unsafeBackpermute" forall f f' (a :: forall a. Array a)). unsafeBackpermute f (unsafeBackpermute f' a) == unsafeBackpermute (f . f') a
-}

-- | Unwrap a scalar.
--
-- >>> s = array @'[] @Int [3]
-- >>> :t fromScalar s
-- fromScalar s :: Int
fromScalar :: (VG.Vector v a) => Array v '[] a -> a
fromScalar a = index a (UnsafeFins [])

-- | Wrap a scalar.
--
-- >>> :t toScalar @Int 2
-- toScalar @Int 2 :: Array '[] Int
toScalar :: (VG.Vector v a) => a -> Array v '[] a
toScalar a = Array (VG.singleton a)

-- | Is an array a scalar?
--
-- >>> isScalar (toScalar (2::Int))
-- True
isScalar :: (KnownNats s, VG.Vector v a) => Array v s a -> Bool
isScalar a = rank a == 0

-- | Convert a scalar to being a dimensioned array. Do nothing if not a scalar.
--
-- >>> asSingleton (toScalar 4)
-- [4]
asSingleton :: (KnownNats s, KnownNats s', s' ~ Eval (AsSingleton s), VG.Vector v a) => Array v s a -> Array v s' a
asSingleton = unsafeModifyShape

-- | Convert an array with shape [1] to being a scalar (Do nothing if not a shape [1] array).
--
-- >>> pretty (asScalar (singleton 3))
-- 3
asScalar :: (KnownNats s, KnownNats s', s' ~ Eval (AsScalar s), VG.Vector v a) => Array v s a -> Array v s' a
asScalar = unsafeModifyShape

-- | An array with no elements.
--
-- >>> toDynamic empty
-- UnsafeArray [0] []
empty :: (VG.Vector v a) => Array v '[0] a
empty = array []

-- | An enumeration of row-major or [lexicographic](https://en.wikipedia.org/wiki/Lexicographic_order) order.
--
-- >>> pretty (range :: Array [2,3] Int)
-- [[0,1,2],
--  [3,4,5]]
range :: forall v s. (KnownNats s, VG.Vector v Int) => Array v s Int
range = tabulate (S.flatten (VU.fromList (valuesOf @s)) . VU.fromList . fromFins)

-- | An enumeration of col-major or [colexicographic](https://en.wikipedia.org/wiki/Lexicographic_order) order.
--
-- >>> pretty (corange @[2,3,4])
-- [[[0,6,12,18],
--   [2,8,14,20],
--   [4,10,16,22]],
--  [[1,7,13,19],
--   [3,9,15,21],
--   [5,11,17,23]]]
corange :: forall v s. (KnownNats s, VG.Vector v Int) => Array v s Int
corange = tabulate (S.flatten (VU.fromList (List.reverse (valuesOf @s))) . VU.fromList . List.reverse . fromFins)

-- | Indices of an array shape.
--
-- >>> pretty $ indices @[3,3]
-- [[[0,0],[0,1],[0,2]],
--  [[1,0],[1,1],[1,2]],
--  [[2,0],[2,1],[2,2]]]
indices :: (KnownNats s, VG.Vector v [Int]) => Array v s [Int]
indices = tabulate fromFins

-- | The identity array.
--
-- >>> pretty $ ident @[3,3]
-- [[1,0,0],
--  [0,1,0],
--  [0,0,1]]
ident :: (KnownNats s, Add.Additive a, Mult.Multiplicative a, VG.Vector v a) => Array v s a
ident = tabulate (bool Add.zero Mult.one . S.isDiag . VU.fromList . fromFins)

-- | Create an array composed of a single value.
--
-- >>> pretty $ konst @[3,2] 1
-- [[1,1],
--  [1,1],
--  [1,1]]
konst :: (KnownNats s, VG.Vector v a) => a -> Array v s a
konst a = tabulate (const a)

-- | Create an array of shape [1].
--
-- >>> pretty $ singleton 1
-- [1]
singleton :: forall v a. (VG.Vector v a) => a -> Array v '[1] a
singleton a = Array (VG.singleton @v a)

-- | Extract the diagonal of an array.
--
-- >>> pretty $ diag (ident @[3,3])
-- [1,1,1]
diag :: forall v s' a s. (KnownNats s, KnownNats s', s' ~ Eval (MinDim s), VG.Vector v a) => Array v s a -> Array v s' a
diag a = unsafeBackpermute (replicate (rank a) . S.getDimL 0) a

-- | Expand an array to form a diagonal array
--
-- >>> pretty $ undiag (range @'[3])
-- [[0,0,0],
--  [0,1,0],
--  [0,0,2]]
undiag :: forall v s' a s. (KnownNats s, KnownNats s', s' ~ Eval ((++) s s), Add.Additive a, VG.Vector v a) => Array v s a -> Array v s' a
undiag a = tabulate (\xs -> bool Add.zero (index a (UnsafeFins $ pure $ S.getDimL 0 (fromFins xs))) (S.isDiagL (fromFins xs)))

-- | Zip two arrays at an element level.
--
-- >>> zipWith (-) v v
-- [0,0,0]
zipWith :: (KnownNats s, VG.Vector v a, VG.Vector v b, VG.Vector v c) => (a -> b -> c) -> Array v s a -> Array v s b -> Array v s c
zipWith f (Array a) (Array b) = Array (VG.zipWith f a b)

-- | Modify a single value at an index.
--
-- >>> pretty $ modify (S.UnsafeFins [0,0]) (const 100) (range @[3,2])
-- [[100,1],
--  [2,3],
--  [4,5]]
modify :: (KnownNats s, VG.Vector v a) => Fins s -> (a -> a) -> Array v s a -> Array v s a
modify ds f a = tabulate (\s -> bool id f (s == ds) (index a s))

-- | Maps an index function at element-level.
--
-- >>> pretty $ imap (\xs x -> x - sum xs) a
-- [[[0,0,0,0],
--   [3,3,3,3],
--   [6,6,6,6]],
--  [[11,11,11,11],
--   [14,14,14,14],
--   [17,17,17,17]]]
imap :: (KnownNats s, VG.Vector v a, VG.Vector v b, VG.Vector v [Int]) => ([Int] -> a -> b) -> Array v s a -> Array v s b
imap f a = zipWith f indices a

-- | With a function that takes dimensions and (type-level) parameters, apply the parameters to the initial dimensions. ie
--
-- > rowWise f xs = f [0..rank xs - 1] xs
--
-- >>> toDynamic $ rowWise indexesT (S.SNats @[1,0]) a
-- UnsafeArray [4] [12,13,14,15]
rowWise :: forall v a ds s s' xs proxy. (KnownNats s, KnownNats ds, ds ~ Eval (DimsOf xs), VG.Vector v a) => (Dims ds -> proxy xs -> Array v s a -> Array v s' a) -> proxy xs -> Array v s a -> Array v s' a
rowWise f xs a = f (Dims @ds) xs a

-- | With a function that takes dimensions and (type-level) parameters, apply the parameters to the the last dimensions. ie
--
-- > colWise f xs = f (List.reverse [0 .. (rank a - 1)]) xs
--
-- >>> toDynamic $ colWise indexesT (S.SNats @[1,0]) a
-- UnsafeArray [2] [1,13]
colWise :: forall v a ds s s' xs proxy. (KnownNats s, KnownNats ds, ds ~ Eval (EndDimsOf xs s), VG.Vector v a) => (Dims ds -> proxy xs -> Array v s a -> Array v s' a) -> proxy xs -> Array v s a -> Array v s' a
colWise f xs a = f (Dims @ds) xs a

-- | Take the top-most elements across the specified dimension.
--
-- >>> pretty $ take (Dim @2) (SNat @1) a
-- [[[0],
--   [4],
--   [8]],
--  [[12],
--   [16],
--   [20]]]
take :: forall v d t s s' a. (KnownNats s, KnownNats s', s' ~ Eval (TakeDim d t s), VG.Vector v a) => Dim d -> SNat t -> Array v s a -> Array v s' a
take _ _ a = unsafeBackpermute id a

-- | Take the bottom-most elements across the specified dimension.
--
-- >>> pretty $ takeB (Dim @2) (SNat @1) a
-- [[[3],
--   [7],
--   [11]],
--  [[15],
--   [19],
--   [23]]]
takeB :: forall v s s' a d t. (KnownNats s, KnownNats s', s' ~ Eval (TakeDim d t s), VG.Vector v a) => Dim d -> SNat t -> Array v s a -> Array v s' a
takeB Dim SNat a = unsafeBackpermute (S.modifyDimL (valueOf @d) (\x -> x + S.getDimL (valueOf @d) (VU.toList (shape a)) - (valueOf @t))) a

-- | Drop the top-most elements across the specified dimension.
--
-- >>> pretty $ drop (Dim @2) (SNat @1) a
-- [[[1,2,3],
--   [5,6,7],
--   [9,10,11]],
--  [[13,14,15],
--   [17,18,19],
--   [21,22,23]]]
drop :: forall v s s' a d t. (KnownNats s, KnownNats s', Eval (DropDim d t s) ~ s', VG.Vector v a) => Dim d -> SNat t -> Array v s a -> Array v s' a
drop Dim SNat a = unsafeBackpermute (S.modifyDimL (valueOf @d) (\x -> x + valueOf @t)) a

-- | Drop the bottom-most elements across the specified dimension.
--
-- >>> pretty $ dropB (Dim @2) (SNat @1) a
-- [[[0,1,2],
--   [4,5,6],
--   [8,9,10]],
--  [[12,13,14],
--   [16,17,18],
--   [20,21,22]]]
dropB :: forall v s s' a d t. (KnownNats s, KnownNats s', Eval (DropDim d t s) ~ s', VG.Vector v a) => Dim d -> SNat t -> Array v s a -> Array v s' a
dropB _ _ a = unsafeBackpermute id a

-- ---------------------------------------------------------------------------
-- Matrix block functions
-- ---------------------------------------------------------------------------

-- | Select an index along a dimension.
--
-- >>> let s = select (Dim @2) (S.fin @4 3) a
-- >>> pretty s
-- [[3,7,11],
--  [15,19,23]]
select :: forall v d a p s s'. (KnownNats s, KnownNats s', s' ~ Eval (DeleteDim d s), p ~ Eval (GetDim d s), VG.Vector v a) => Dim d -> Fin p -> Array v s a -> Array v s' a
select Dim p a = unsafeBackpermute (S.insertDimL (valueOf @d) (fromFin p)) a

-- | Insert along a dimension at a position.
--
-- >>> pretty $ insert (Dim @2) (UnsafeFin 0) a (konst @[2,3] 0)
-- [[[0,0,1,2,3],
--   [0,4,5,6,7],
--   [0,8,9,10,11]],
--  [[0,12,13,14,15],
--   [0,16,17,18,19],
--   [0,20,21,22,23]]]
-- >>> toDynamic $ insert (Dim @0) (UnsafeFin 0) (toScalar 1) (toScalar 2)
-- UnsafeArray [2] [2,1]
insert :: forall v s' s si d p a. (KnownNats s, KnownNats si, KnownNats s', s' ~ Eval (IncAt d s), p ~ Eval (GetDim d s), True ~ Eval (InsertOk d s si), VG.Vector v a) => Dim d -> Fin p -> Array v s a -> Array v si a -> Array v s' a
insert Dim i a b = tabulate go
  where
    go s
      | S.getDimL d s' == fromFin i = index b (UnsafeFins (S.deleteDimL d s'))
      | S.getDimL d s' < fromFin i = index a (UnsafeFins s')
      | otherwise = index a (UnsafeFins (S.decAtL d s'))
      where
        s' = fromFins s
    d = valueOf @d

-- | Delete along a dimension at a position.
--
-- >>> pretty $ delete (Dim @2) (UnsafeFin 3) a
-- [[[0,1,2],
--   [4,5,6],
--   [8,9,10]],
--  [[12,13,14],
--   [16,17,18],
--   [20,21,22]]]
delete :: forall v d s s' p a. (KnownNats s, KnownNats s', s' ~ Eval (DecAt d s), p ~ 1 + Eval (GetDim d s), VG.Vector v a) => Dim d -> Fin p -> Array v s a -> Array v s' a
delete Dim p a = unsafeBackpermute (\s -> bool (S.incAtL d s) s (S.getDimL d s < fromFin p)) a
  where
    d = valueOf @d

-- | Insert along a dimension at the end.
--
-- >>> pretty $ append (Dim @2) a (konst @[2,3] 0)
-- [[[0,1,2,3,0],
--   [4,5,6,7,0],
--   [8,9,10,11,0]],
--  [[12,13,14,15,0],
--   [16,17,18,19,0],
--   [20,21,22,23,0]]]
append :: forall v a d s si s'. (KnownNats s, KnownNats si, KnownNats s', s' ~ Eval (IncAt d s), True ~ Eval (InsertOk d s si), VG.Vector v a) => Dim d -> Array v s a -> Array v si a -> Array v s' a
append (Dim :: Dim d) = insert (Dim @d) (UnsafeFin (S.getDimL (valueOf @d) (valuesOf @s)))

-- | Insert along a dimension at the beginning.
--
-- >>> pretty $ prepend (Dim @2) (konst @[2,3] 0) a
-- [[[0,0,1,2,3],
--   [0,4,5,6,7],
--   [0,8,9,10,11]],
--  [[0,12,13,14,15],
--   [0,16,17,18,19],
--   [0,20,21,22,23]]]
prepend :: forall v a d s si s'. (KnownNats s, KnownNats si, KnownNats s', s' ~ Eval (IncAt d s), True ~ Eval (InsertOk d s si), VG.Vector v a) => Dim d -> Array v si a -> Array v s a -> Array v s' a
prepend d a b = insert d (UnsafeFin 0) b a

-- | Concatenate along a dimension.
--
-- >>> shape $ concatenate (Dim @1) a a
-- [2,6,4]
-- >>> toDynamic $ concatenate (Dim @0) (toScalar 1) (toScalar 2)
-- UnsafeArray [2] [1,2]
-- >>> toDynamic $ concatenate (Dim @0) (array @'[1] [0]) (array @'[3] [1..3])
-- UnsafeArray [4] [0,1,2,3]
concatenate :: forall v a s0 s1 d s. (KnownNats s0, KnownNats s1, KnownNats s, Eval (Concatenate d s0 s1) ~ s, VG.Vector v a) => Dim d -> Array v s0 a -> Array v s1 a -> Array v s a
concatenate Dim a0 a1 = tabulate (go . fromFins)
  where
    go s =
      bool
        (index a0 (UnsafeFins s))
        ( index
            a1
            ( UnsafeFins $
                S.insertDimL
                  d'
                  (S.getDimL d' s - S.getDimL d' ds0)
                  (S.deleteDimL d' s)
            )
        )
        (S.getDimL d' s >= S.getDimL d' ds0)
    ds0 = VU.toList (shape a0)
    d' = valueOf @d

-- | Combine two arrays as a new dimension of a new array.
--
-- >>> pretty $ couple (Dim @0) (array @'[3] [1,2,3]) (array @'[3] @Int [4,5,6])
-- [[1,2,3],
--  [4,5,6]]
-- >>> couple (Dim @0) (toScalar @Int 0) (toScalar 1)
-- [0,1]
couple :: forall v d a s s' se. (KnownNat d, KnownNats s, KnownNats s', KnownNats se, s' ~ Eval (Concatenate d se se), se ~ Eval (InsertDim d 1 s), VG.Vector v a) => Dim d -> Array v s a -> Array v s a -> Array v s' a
couple d a a' = concatenate d (elongate d a) (elongate d a')

-- | Slice along a dimension with the supplied offset & length.
--
-- >>> pretty $ slice (Dim @2) (SNat @1) (SNat @2) a
-- [[[1,2],
--   [5,6],
--   [9,10]],
--  [[13,14],
--   [17,18],
--   [21,22]]]
slice :: forall v a d off l s s'. (KnownNats s, KnownNats s', s' ~ Eval (SetDim d l s), Eval (SliceOk d off l s) ~ True, VG.Vector v a) => Dim d -> SNat off -> SNat l -> Array v s a -> Array v s' a
slice Dim SNat _ a = unsafeBackpermute (S.modifyDimL (valueOf @d) (+ (valueOf @off))) a

-- | Rotate an array along a dimension.
--
-- >>> pretty $ rotate (Dim @1) 2 a
-- [[[8,9,10,11],
--   [0,1,2,3],
--   [4,5,6,7]],
--  [[20,21,22,23],
--   [12,13,14,15],
--   [16,17,18,19]]]
rotate :: forall v d s a. (KnownNats s, VG.Vector v a) => Dim d -> Int -> Array v s a -> Array v s a
rotate Dim r a = unsafeBackpermute (S.rotateIndexL (valueOf @d) r (VU.toList (shape a))) a

-- * multi-dimensional operators

-- | Across the specified dimensions, takes the top-most elements.
--
-- >>> pretty $ takes (Dims @[0,1]) (S.SNats @[1,2]) a
-- [[[0,1,2,3],
--   [4,5,6,7]]]
takes :: forall v ds xs s' s a. (KnownNats s, KnownNats s', s' ~ Eval (SetDims ds xs s), VG.Vector v a) => Dims ds -> SNats xs -> Array v s a -> Array v s' a
takes _ _ a = unsafeBackpermute id a

-- | Across the specified dimensions, takes the bottom-most elements.
--
-- >>> pretty (takeBs (Dims @[0,1]) (S.SNats @[1,2]) a)
-- [[[16,17,18,19],
--   [20,21,22,23]]]
takeBs :: forall v s' s a ds xs. (KnownNats s, KnownNats s', KnownNats ds, KnownNats xs, s' ~ Eval (SetDims ds xs s), VG.Vector v a) => Dims ds -> SNats xs -> Array v s a -> Array v s' a
takeBs _ _ a = unsafeBackpermute (List.zipWith (+) start) a
  where
    start = List.zipWith (-) (VU.toList (shape a)) (S.setDimsL (valuesOf @ds) (valuesOf @xs) (VU.toList (shape a)))

-- | Across the specified dimensions, drops the top-most elements.
--
-- >>> pretty $ drops (Dims @[0,2]) (S.SNats @[1,3]) a
-- [[[15],
--   [19],
--   [23]]]
drops :: forall v ds xs s' s a. (KnownNats s, KnownNats s', KnownNats ds, KnownNats xs, s' ~ Eval (DropDims ds xs s), VG.Vector v a) => Dims ds -> SNats xs -> Array v s a -> Array v s' a
drops _ _ a = unsafeBackpermute (List.zipWith (+) start) a
  where
    start = List.zipWith (-) (valuesOf @s) (valuesOf @s')

-- | Across the specified dimensions, drops the bottom-most elements.
--
-- >>> pretty $ dropBs (Dims @[0,2]) (S.SNats @[1,3]) a
-- [[[0],
--   [4],
--   [8]]]
dropBs :: forall v s' s ds xs a. (KnownNats s, KnownNats s', KnownNats ds, KnownNats xs, s' ~ Eval (DropDims ds xs s), VG.Vector v a) => Dims ds -> SNats xs -> Array v s a -> Array v s' a
dropBs _ _ a = unsafeBackpermute id a

-- | Select by dimensions and indexes.
--
-- >>> pretty $ indexes (Dims @[0,1]) (S.UnsafeFins [1,1]) a
-- [16,17,18,19]
-- >>> F.indexes (S.SNats @'[1]) (S.fins @'[3] [1]) (F.range @[2,3])
-- [1,4]
indexes :: forall v s' s ds xs a. (KnownNats s, KnownNats s', s' ~ Eval (DeleteDims ds s), xs ~ Eval (GetDims ds s), VG.Vector v a) => Dims ds -> Fins xs -> Array v s a -> Array v s' a
indexes Dims xs a = unsafeBackpermute (S.insertDimsL (valuesOf @ds) (fromFins xs)) a

-- | Select by dimensions and indexes, supplying indexes as a type.
--
-- >>> pretty $ indexesT (Dims @[0,1]) (S.SNats @[1,1]) a
-- [16,17,18,19]
indexesT :: forall v ds xs s s' a. (KnownNats s, KnownNats ds, KnownNats xs, KnownNats s', s' ~ Eval (DeleteDims ds s), True ~ Eval (IsFins xs =<< GetDims ds s), VG.Vector v a) => Dims ds -> SNats xs -> Array v s a -> Array v s' a
indexesT ds _ a = indexes ds (UnsafeFins $ valuesOf @xs) a

-- | Slice along dimensions with the supplied offsets and lengths.
--
-- >>> pretty $ slices (Dims @'[2]) (S.SNats @'[1]) (S.SNats @'[2]) a
-- [[[1,2],
--   [5,6],
--   [9,10]],
--  [[13,14],
--   [17,18],
--   [21,22]]]
slices :: forall v a ds ls offs s s'. (KnownNats s, KnownNats s', KnownNats ds, KnownNats ls, KnownNats offs, Eval (SlicesOk ds offs ls s) ~ True, Eval (SetDims ds ls s) ~ s', VG.Vector v a) => Dims ds -> SNats offs -> SNats ls -> Array v s a -> Array v s' a
slices _ _ _ a = unsafeBackpermute (List.zipWith (+) o) a
  where
    o = S.setDimsL (valuesOf @ds) (valuesOf @offs) (replicate (rank a) 0)

-- | Select the first element along the supplied dimensions.
--
-- >>> pretty $ heads (Dims @[0,2]) a
-- [0,4,8]
heads :: forall v a ds s s'. (KnownNats s, KnownNats s', KnownNats ds, s' ~ Eval (DeleteDims ds s), VG.Vector v a) => Dims ds -> Array v s a -> Array v s' a
heads ds a = indexes ds (UnsafeFins $ replicate (rankOf @ds) 0) a

-- | Select the last element along the supplied dimensions.
--
-- >>> pretty $ lasts (Dims @[0,2]) a
-- [15,19,23]
lasts :: forall v ds s s' a. (KnownNats s, KnownNats ds, KnownNats s', s' ~ Eval (DeleteDims ds s), VG.Vector v a) => Dims ds -> Array v s a -> Array v s' a
lasts ds a = indexes ds (UnsafeFins lastds) a
  where
    lastds = (\i -> S.getDimL i (VU.toList (shape a)) - 1) <$> (valuesOf @ds)

-- | Select the tail elements along the supplied dimensions.
--
-- >>> pretty $ tails (Dims @[0,2]) a
-- [[[13,14,15],
--   [17,18,19],
--   [21,22,23]]]
tails :: forall v ds os s s' a ls. (KnownNats s, KnownNats ds, KnownNats s', KnownNats ls, KnownNats os, Eval (SlicesOk ds os ls s) ~ True, os ~ Eval (Replicate (Eval (Rank ds)) 1), ls ~ Eval (GetLastPositions ds s), s' ~ Eval (SetDims ds ls s), VG.Vector v a) => Dims ds -> Array v s a -> Array v s' a
tails ds a = slices ds (SNats @os) (SNats @ls) a

-- | Select the init elements along the supplied dimensions.
--
-- >>> pretty $ inits (Dims @[0,2]) a
-- [[[0,1,2],
--   [4,5,6],
--   [8,9,10]]]
inits :: forall v ds os s s' a ls. (KnownNats s, KnownNats ds, KnownNats s', KnownNats ls, KnownNats os, Eval (SlicesOk ds os ls s) ~ True, os ~ Eval (Replicate (Eval (Rank ds)) 0), ls ~ Eval (GetLastPositions ds s), s' ~ Eval (SetDims ds ls s), VG.Vector v a) => Dims ds -> Array v s a -> Array v s' a
inits ds a = slices ds (SNats @os) (SNats @ls) a

-- | Extracts specified dimensions to an outer layer.
--
-- >>> :t extracts (Dims @'[0]) (range @[2,3,4])
-- extracts (Dims @'[0]) (range @[2,3,4])
--   :: Array v '[2] (Array [3, 4] Int)
extracts :: forall v ds st si so a. (KnownNats st, KnownNats ds, KnownNats si, KnownNats so, si ~ Eval (DeleteDims ds st), so ~ Eval (GetDims ds st), VG.Vector v a, VG.Vector v (Array v si a)) => Dims ds -> Array v st a -> Array v so (Array v si a)
extracts ds a = tabulate (\s -> indexes ds s a)

-- | Reduce along specified dimensions, using the supplied fold.
--
-- >>> pretty $ reduces (Dims @'[0]) sum a
-- [66,210]
-- >>> pretty $ reduces (Dims @[0,2]) sum a
-- [[12,15,18,21],
--  [48,51,54,57]]
reduces :: forall v ds st si so a b. (KnownNats st, KnownNats ds, KnownNats si, KnownNats so, si ~ Eval (DeleteDims ds st), so ~ Eval (GetDims ds st), VG.Vector v a, VG.Vector v b, VG.Vector v (Array v si a)) => Dims ds -> (Array v si a -> b) -> Array v st a -> Array v so b
reduces ds f a = fmapA f (extracts ds a)

-- | Join inner and outer dimension layers by supplied dimensions.
--
-- >>> let e = extracts (Dims @[1,0]) a
-- >>> let j = joins (Dims @[1,0]) e
-- >>> a == j
-- True
joins :: forall v a ds si so st. (KnownNats ds, KnownNats st, KnownNats si, KnownNats so, Eval (InsertDims ds so si) ~ st, VG.Vector v (Array v si a), VG.Vector v a) => Dims ds -> Array v so (Array v si a) -> Array v st a
joins _ a = tabulate go
  where
    go s = index (index a (UnsafeFins $ S.getDimsL (valuesOf @ds) (fromFins s))) (UnsafeFins $ S.deleteDimsL (valuesOf @ds) (fromFins s))

-- | Join inner and outer dimension layers in outer dimension order.
--
-- >>> a == join (extracts (Dims @[0,1]) a)
-- True
join :: forall v a si so st ds. (KnownNats st, KnownNats si, KnownNats so, KnownNats ds, ds ~ Eval (DimsOf so), st ~ Eval (InsertDims ds so si), VG.Vector v (Array v si a), VG.Vector v a) => Array v so (Array v si a) -> Array v st a
join a = joins (SNats @ds) a

-- | Traverse along specified dimensions.
--
-- >>> traverses (Dims @'[1]) print (range @[2,3])
-- 0
-- 3
-- 1
-- 4
-- 2
-- 5
-- [(),(),(),(),(),()]
traverses :: (Applicative f, KnownNats s, KnownNats si, KnownNats so, si ~ Eval (GetDims ds s), so ~ Eval (DeleteDims ds s), s ~ Eval (InsertDims ds si so), VG.Vector v a, VG.Vector v b, VG.Vector v (Array v so a), VG.Vector v (Array v so b), VG.Vector v (f b), VG.Vector v (f (Array v so b))) => Dims ds -> (a -> f b) -> Array v s a -> f (Array v s b)
traverses (Dims :: Dims ds) f a = joins (SNats @ds) <$> traverseA (traverseA f) (extracts (Dims :: Dims ds) a)

-- | Maps a function along specified dimensions.
--
-- >>> pretty $ maps (Dims @'[1]) transpose a
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
maps :: forall v ds s s' si si' so a b. (KnownNats s, KnownNats s', KnownNats si, KnownNats si', KnownNats so, si ~ Eval (DeleteDims ds s), so ~ Eval (GetDims ds s), s' ~ Eval (InsertDims ds so si'), s ~ Eval (InsertDims ds so si), VG.Vector v a, VG.Vector v b, VG.Vector v (Array v si a), VG.Vector v (Array v si' b)) => Dims ds -> (Array v si a -> Array v si' b) -> Array v s a -> Array v s' b
maps SNats f a = joins (SNats @ds) (fmapA f (extracts (SNats @ds) a))

-- | Filters along specified dimensions (which are flattened as a dynamic array).
--
-- >>> pretty $ filters (Dims @[0,1]) (any ((==0) . (`mod` 7))) a
-- [[0,1,2,3],[4,5,6,7],[12,13,14,15],[20,21,22,23]]
filters :: forall v ds si so a. (KnownNats si, KnownNats so, si ~ Eval (DeleteDims ds so), KnownNats (Eval (GetDims ds so)), VG.Vector v a, VG.Vector v (Array v si a)) => Dims ds -> (Array v si a -> Bool) -> Array v so a -> A.Array v (Array v si a)
filters Dims p a =
  let v' = VG.filter p (asVector (extracts (Dims @ds) a))
   in A.arrayV [VG.length v'] v'

-- | Zips two arrays with a function along specified dimensions.
--
-- >>> pretty $ zips (Dims @[0,1]) (zipWith (,)) a (reverses (Dims @'[0]) a)
-- [[[(0,12),(1,13),(2,14),(3,15)],
--   [(4,16),(5,17),(6,18),(7,19)],
--   [(8,20),(9,21),(10,22),(11,23)]],
--  [[(12,0),(13,1),(14,2),(15,3)],
--   [(16,4),(17,5),(18,6),(19,7)],
--   [(20,8),(21,9),(22,10),(23,11)]]]
zips :: forall v ds s s' si si' so a b c. (KnownNats s, KnownNats s', KnownNats si, KnownNats si', KnownNats so, si ~ Eval (DeleteDims ds s), so ~ Eval (GetDims ds s), s' ~ Eval (InsertDims ds so si'), s ~ Eval (InsertDims ds so si), VG.Vector v a, VG.Vector v b, VG.Vector v c, VG.Vector v (Array v si a), VG.Vector v (Array v si b), VG.Vector v (Array v si' c)) => Dims ds -> (Array v si a -> Array v si b -> Array v si' c) -> Array v s a -> Array v s b -> Array v s' c
zips SNats f a b = joins (Dims @ds) (zipWith f (extracts (Dims @ds) a) (extracts (Dims @ds) b))

-- | Modify using the supplied function along dimensions and positions.
--
-- >>> pretty $ modifies (fmap (100+)) (Dims @'[2]) (S.UnsafeFins [0]) a
-- [[[100,1,2,3],
--   [104,5,6,7],
--   [108,9,10,11]],
--  [[112,13,14,15],
--   [116,17,18,19],
--   [120,21,22,23]]]
modifies :: forall v a si s ds so. (KnownNats s, KnownNats si, KnownNats so, si ~ Eval (DeleteDims ds s), so ~ Eval (GetDims ds s), s ~ Eval (InsertDims ds so si), VG.Vector v a, VG.Vector v (Array v si a)) => (Array v si a -> Array v si a) -> Dims ds -> Fins so -> Array v s a -> Array v s a
modifies f SNats ps a = joins (Dims @ds) $ modify ps f (extracts (Dims @ds) a)

-- | Apply a binary function between successive slices, across dimensions and lags.
--
-- >>> pretty $ diffs (Dims @'[1]) (S.SNats @'[1]) (zipWith (-)) a
-- [[[4,4,4,4],
--   [4,4,4,4]],
--  [[4,4,4,4],
--   [4,4,4,4]]]
diffs :: forall v a b ds ls si si' st st' so postDrop. (KnownNats ls, KnownNats si, KnownNats si', KnownNats st, KnownNats st', KnownNats so, KnownNats postDrop, si ~ Eval (DeleteDims ds postDrop), so ~ Eval (GetDims ds postDrop), st' ~ Eval (InsertDims ds so si'), postDrop ~ Eval (InsertDims ds so si), postDrop ~ Eval (DropDims ds ls st), VG.Vector v a, VG.Vector v b, VG.Vector v (Array v si a), VG.Vector v (Array v si' b)) => Dims ds -> SNats ls -> (Array v si a -> Array v si a -> Array v si' b) -> Array v st a -> Array v st' b
diffs SNats xs f a = zips (Dims @ds) f (drops (Dims @ds) xs a) (dropBs (Dims @ds) xs a)

-- | Product two arrays using the supplied binary function.
--
-- For context, if the function is multiply, and the arrays are tensors,
-- then this can be interpreted as a [tensor product](https://en.wikipedia.org/wiki/Tensor_product).
-- The concept of a tensor product is a dense crossroad, and a complete treatment is elsewhere.  To quote the wiki article:
--
-- ... the tensor product can be extended to other categories of mathematical objects in addition to vector spaces, such as to matrices, tensors, algebras, topological vector spaces, and modules. In each such case the tensor product is characterized by a similar universal property: it is the freest bilinear operation. The general concept of a "tensor product" is captured by monoidal categories; that is, the class of all things that have a tensor product is a monoidal category.
--
-- >>> x = array [1,2,3] :: Array '[3] Int
-- >>> pretty $ expand (*) x x
-- [[1,2,3],
--  [2,4,6],
--  [3,6,9]]
--
-- Alternatively, expand can be understood as representing the permutation of element pairs of two arrays, so like the Applicative List instance.
--
-- >>> i2 = indices @[2,2]
-- >>> pretty $ expand (,) i2 i2
-- [[[[([0,0],[0,0]),([0,0],[0,1])],
--    [([0,0],[1,0]),([0,0],[1,1])]],
--   [[([0,1],[0,0]),([0,1],[0,1])],
--    [([0,1],[1,0]),([0,1],[1,1])]]],
--  [[[([1,0],[0,0]),([1,0],[0,1])],
--    [([1,0],[1,0]),([1,0],[1,1])]],
--   [[([1,1],[0,0]),([1,1],[0,1])],
--    [([1,1],[1,0]),([1,1],[1,1])]]]]
expand :: forall v sc sa sb a b c. (KnownNats sa, KnownNats sb, KnownNats sc, sc ~ Eval ((++) sa sb), VG.Vector v a, VG.Vector v b, VG.Vector v c) => (a -> b -> c) -> Array v sa a -> Array v sb b -> Array v sc c
expand f a b = tabulate (\i -> f (index a (UnsafeFins $ List.take r (fromFins i))) (index b (UnsafeFins $ List.drop r (fromFins i))))
  where
    r = rank a

-- | Like expand, but permutes the first array first, rather than the second.
--
-- >>> pretty $ expand (,) v (fmap (+3) v)
-- [[(0,3),(0,4),(0,5)],
--  [(1,3),(1,4),(1,5)],
--  [(2,3),(2,4),(2,5)]]
--
-- >>> pretty $ coexpand (,) v (fmap (+3) v)
-- [[(0,3),(1,3),(2,3)],
--  [(0,4),(1,4),(2,4)],
--  [(0,5),(1,5),(2,5)]]
coexpand :: forall v sc sa sb a b c. (KnownNats sa, KnownNats sb, KnownNats sc, sc ~ Eval ((++) sb sa), VG.Vector v a, VG.Vector v b, VG.Vector v c) => (a -> b -> c) -> Array v sa a -> Array v sb b -> Array v sc c
coexpand f a b = tabulate (\i -> f (index a (UnsafeFins $ List.drop rb (fromFins i))) (index b (UnsafeFins $ List.take rb (fromFins i))))
  where
    rb = rank b

-- | Contract an array by applying the supplied (folding) function on diagonal elements of the dimensions.
--
-- This generalises a tensor contraction by allowing the number of contracting diagonals to be other than 2.
--
--
-- >>> pretty $ contract (Dims @[1,2]) sum (expand (*) m (transpose m))
-- [[5,14],
--  [14,50]]
contract :: forall v a b s ss se s' ds ds'. (KnownNats se, se ~ Eval (DeleteDims ds' s), KnownNats ds', KnownNats s, KnownNats ss, KnownNats s', s' ~ Eval (GetDims ds' s), ss ~ Eval (MinDim se), ds' ~ Eval (ExceptDims ds s), VG.Vector v a, VG.Vector v b, VG.Vector v (Array v se a)) => Dims ds -> (Array v ss a -> b) -> Array v s a -> Array v s' b
contract SNats f a = fmapA (f . diag) (extracts (Dims @ds') a)

-- | Expand two arrays and then contract the result using the supplied matching dimensions.
--
-- >>> pretty $ prod (Dims @'[1]) (Dims @'[0]) sum (*) (range @[2,3]) (range @[3,2])
-- [[10,13],
--  [28,40]]
--
-- With full laziness, this computation would be equivalent to:
--
-- > f . diag <$> extracts (Dims @ds') (expand g a b)
prod :: forall v a b c d s0 s1 so0 so1 si st ds0 ds1. (KnownNats so0, KnownNats so1, KnownNats si, KnownNats s0, KnownNats s1, KnownNats st, KnownNats ds0, KnownNats ds1, so0 ~ Eval (DeleteDims ds0 s0), so1 ~ Eval (DeleteDims ds1 s1), si ~ Eval (GetDims ds0 s0), si ~ Eval (GetDims ds1 s1), st ~ Eval ((++) so0 so1), VG.Vector v c, VG.Vector v a, VG.Vector v b, VG.Vector v d) => Dims ds0 -> Dims ds1 -> (Array v si c -> d) -> (a -> b -> c) -> Array v s0 a -> Array v s1 b -> Array v st d
prod SNats SNats g f a b = unsafeTabulate (\so -> g $ unsafeTabulate (\si -> f (unsafeIndex a (S.insertDimsL (valuesOf @ds0) si (List.take sp so))) (unsafeIndex b (S.insertDimsL (valuesOf @ds1) si (List.drop sp so)))))
  where
    sp = rank a - rankOf @ds0

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
-- Note that an Array with shape [3] is neither a row vector nor column vector.
--
-- >>> pretty $ dot sum (*) v (transpose m)
-- [5,14]
--
-- >>> pretty $ dot sum (*) m v
-- [5,14]
dot :: forall v a b c d ds0 ds1 s0 s1 so0 so1 st si. (KnownNats s0, KnownNats s1, KnownNats ds0, KnownNats ds1, KnownNats so0, KnownNats so1, KnownNats st, KnownNats si, so0 ~ Eval (DeleteDims ds0 s0), so1 ~ Eval (DeleteDims ds1 s1), si ~ Eval (GetDims ds0 s0), si ~ Eval (GetDims ds1 s1), st ~ Eval ((++) so0 so1), ds0 ~ '[Eval ((Fcf.-) (Eval (Rank s0)) 1)], ds1 ~ '[0], VG.Vector v c, VG.Vector v a, VG.Vector v b, VG.Vector v d) => (Array v si c -> d) -> (a -> b -> c) -> Array v s0 a -> Array v s1 b -> Array v st d
dot f g a b = prod (Dims @ds0) (Dims @ds1) f g a b

-- | Array multiplication.
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
mult :: forall v a ds0 ds1 s0 s1 so0 so1 st si. (Add.Additive a, Mult.Multiplicative a, KnownNats s0, KnownNats s1, KnownNats ds0, KnownNats ds1, KnownNats so0, KnownNats so1, KnownNats st, KnownNats si, so0 ~ Eval (DeleteDims ds0 s0), so1 ~ Eval (DeleteDims ds1 s1), si ~ Eval (GetDims ds0 s0), si ~ Eval (GetDims ds1 s1), st ~ Eval ((++) so0 so1), ds0 ~ '[Eval ((Fcf.-) (Eval (Rank s0)) 1)], ds1 ~ '[0], VG.Vector v a) => Array v s0 a -> Array v s1 a -> Array v st a
mult = dot sumA (Mult.*)

-- | @windows xs@ are xs-sized windows of an array
--
-- >>> shape $ windows (Dims @[2,2]) (range @[4,3,2])
-- [3,2,2,2,2]
windows :: forall v w s ws a. (KnownNats s, KnownNats ws, ws ~ Eval (ExpandWindows w s), VG.Vector v a) => SNats w -> Array v s a -> Array v ws a
windows SNats a = unsafeBackpermute (S.indexWindowsL (rankOf @w)) a

-- | Find the starting positions of occurences of one array in another.
--
-- >>> a = cycle @[4,4] (range @'[3])
-- >>> i = array @[2,2] [1,2,2,0]
-- >>> pretty $ find i a
-- [[False,True,False],
--  [True,False,False],
--  [False,False,True]]
find :: forall v s' si s a r i' re ws. (Eq a, KnownNats si, KnownNats s, KnownNats s', KnownNats re, KnownNats i', KnownNat r, KnownNats ws, ws ~ Eval (ExpandWindows i' s), r ~ Eval (Rank s), i' ~ Eval (Rerank r si), re ~ Eval (DimWindows ws s), i' ~ Eval (DeleteDims re ws), s' ~ Eval (GetDims re ws), VG.Vector v a, VG.Vector v Bool, Eq (v a), VG.Vector v (Array v i' a)) => Array v si a -> Array v s a -> Array v s' Bool
find i a = xs
  where
    i' = rerank (SNat @r) i
    ws = windows (SNats @i') a
    xs = fmapA (== i') (extracts (SNats @re) ws)

-- | Find the ending positions of one array in another except where the array overlaps with another copy.
--
-- >>> a = konst @[5,5] @Int 1
-- >>> i = konst @[2,2] @Int 1
-- >>> pretty $ findNoOverlap i a
-- [[True,False,True,False],
--  [False,False,False,False],
--  [True,False,True,False],
--  [False,False,False,False]]
findNoOverlap :: forall v s' si s a r i' re ws. (Eq a, KnownNats si, KnownNats s, KnownNats s', KnownNats re, KnownNats i', KnownNat r, KnownNats ws, ws ~ Eval (ExpandWindows i' s), r ~ Eval (Rank s), i' ~ Eval (Rerank r si), re ~ Eval (DimWindows ws s), i' ~ Eval (DeleteDims re ws), s' ~ Eval (GetDims re ws), VG.Vector v a, VG.Vector v Bool, Eq (v a), VG.Vector v (Array v i' a), VG.Vector v [Int]) => Array v si a -> Array v s a -> Array v s' Bool
findNoOverlap i a = r
  where
    f = find i a

    cl :: [Int] -> [[Int]]
    cl sh = List.filter (P.not . any (> 0) . List.init) $ List.filter (P.not . all (>= 0)) $ VG.toList (A.asVector (A.tabulate ((\x -> 2 * x - 1) <$> sh) (\s -> List.zipWith (\x x0 -> x - x0 + 1) s sh) :: A.Array v [Int]))
    go r' s = index f (UnsafeFins s) && not (any (index r' . UnsafeFins) (List.filter (\x -> S.isFinsL x (VU.toList (shape f))) $ fmap (List.zipWith (+) s) (cl (VU.toList (shape i)))))
    r = unsafeTabulate (go r)

-- | Check if the first array is a prefix of the second.
--
-- >>> isPrefixOf (array @[2,2] [0,1,4,5]) a
-- True
isPrefixOf :: forall v s' s r a. (Eq a, KnownNats s, KnownNats s', KnownNat r, KnownNats (Eval (Rerank r s)), True ~ Eval (IsSubset s' s), r ~ Eval (Rank s'), VG.Vector v a, Eq (v a)) => Array v s' a -> Array v s a -> Bool
isPrefixOf p a = p == cut a

-- | Check if the first array is a suffix of the second.
--
-- >>> isSuffixOf (array @[2,2] [18,19,22,23]) a
-- True
isSuffixOf :: forall v s' s r a. (Eq a, KnownNats s, KnownNats s', KnownNat r, KnownNats (Eval (Rerank r s)), r ~ Eval (Rank s'), True ~ Eval (IsSubset s' s), VG.Vector v a, Eq (v a)) => Array v s' a -> Array v s a -> Bool
isSuffixOf p a = p == cutSuffix a

-- | Check if the first array is an infix of the second.
--
-- >>> isInfixOf (array @[2,2] [18,19,22,23]) a
-- True
isInfixOf :: forall v s' si s a r i' re ws. (Eq a, KnownNats si, KnownNats s, KnownNats s', KnownNats re, KnownNats i', KnownNat r, KnownNats ws, ws ~ Eval (ExpandWindows i' s), r ~ Eval (Rank s), i' ~ Eval (Rerank r si), re ~ Eval (DimWindows ws s), i' ~ Eval (DeleteDims re ws), s' ~ Eval (GetDims re ws), VG.Vector v a, VG.Vector v Bool, Eq (v a), VG.Vector v (Array v i' a)) => Array v si a -> Array v s a -> Bool
isInfixOf p a = orA $ find p a

-- | Fill an array with the supplied value without regard to the original shape or cut the array values to match array size.
--
-- > validate (def x a) == True
--
-- >>> pretty $ fill @'[3] 0 (array @'[0] [])
-- [0,0,0]
-- >>> pretty $ fill @'[3] 0 (array @'[4] [1..4])
-- [1,2,3]
fill :: forall v s' a s. (KnownNats s, KnownNats s', VG.Vector v a, Semigroup (v a)) => a -> Array v s a -> Array v s' a
fill x (Array v) = Array (VG.take (S.size (VU.fromList (valuesOf @s'))) (v <> VG.replicate (S.size (VU.fromList (valuesOf @s')) - VG.length v) x))

-- | Cut an array to form a new (smaller) shape. Errors if the new shape is larger. The old array is reranked to the rank of the new shape first.
--
-- >>> toDynamic $ cut @'[2] (array @'[4] @Int [0..3])
-- UnsafeArray [2] [0,1]
cut :: forall v s' s r a. (KnownNats s, KnownNats s', KnownNat r, KnownNats (Eval (Rerank r s)), True ~ Eval (IsSubset s' s), r ~ Eval (Rank s'), VG.Vector v a) => Array v s a -> Array v s' a
cut a = unsafeBackpermute id (rerank (SNat @r) a)

-- | Cut an array to form a new (smaller) shape, using suffix elements. Errors if the new shape is larger. The old array is reranked to the rank of the new shape first.
--
-- >>> toDynamic $ cutSuffix @[2,2] a
-- UnsafeArray [2,2] [18,19,22,23]
cutSuffix :: forall v s' s a r. (KnownNats s, KnownNats s', KnownNat r, KnownNats (Eval (Rerank r s)), r ~ Eval (Rank s'), True ~ Eval (IsSubset s' s), VG.Vector v a) => Array v s a -> Array v s' a
cutSuffix a = unsafeBackpermute (List.zipWith (+) diffDim) a'
  where
    a' = rerank (SNat @r) a
    diffDim = List.zipWith (-) (VU.toList (shape a')) (valuesOf @s')

-- | Pad an array to form a new shape, supplying a default value for elements outside the shape of the old array. The old array is reranked to the rank of the new shape first.
--
-- >>> toDynamic $ pad @'[5] 0 (array @'[4] @Int [0..3])
-- UnsafeArray [5] [0,1,2,3,0]
pad :: forall v s' a s r. (KnownNats s, KnownNats s', KnownNat r, KnownNats (Eval (Rerank r s)), r ~ Eval (Rank s'), VG.Vector v a) => a -> Array v s a -> Array v s' a
pad d a = tabulate (\s -> bool d (index a' (unsafeCoerce s)) (fromFins s `S.isFinsL` VU.toList (shape a')))
  where
    a' = rerank (SNat @r) a

-- | Left pad an array to form a new shape, supplying a default value for elements outside the shape of the old array.
--
-- >>> toDynamic $ lpad @'[5] 0 (array @'[4] [0..3])
-- UnsafeArray [5] [0,0,1,2,3]
-- >>> pretty $ lpad @[3,3] 0 (range @[2,2])
-- [[0,0,0],
--  [0,0,1],
--  [0,2,3]]
lpad :: forall v s' a s r. (KnownNats s, KnownNats s', KnownNat r, KnownNats (Eval (Rerank r s)), r ~ Eval (Rank s'), VG.Vector v a) => a -> Array v s a -> Array v s' a
lpad d a = tabulate (\s -> bool d (index a' (UnsafeFins $ olds s)) (olds s `S.isFinsL` VU.toList (shape a')))
  where
    a' = rerank (SNat @r) a
    gap = List.zipWith (-) (valuesOf @s') (VU.toList (shape a'))
    olds s = List.zipWith (-) (fromFins s) gap

-- | Reshape an array (with the same number of elements).
--
-- >>> pretty $ reshape @[4,3,2] a
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
reshape :: forall v s' s a. (Eval (Size s) ~ Eval (Size s'), KnownNats s, KnownNats s', VG.Vector v a) => Array v s a -> Array v s' a
reshape = unsafeBackpermute (VU.toList . S.shapen (VU.fromList s) . S.flatten (VU.fromList s') . VU.fromList)
  where
    s = valuesOf @s
    s' = valuesOf @s'

-- | Make an Array single dimensional.
--
-- >>> pretty $ flat (range @[2,2])
-- [0,1,2,3]
-- >>> pretty (flat $ toScalar 0)
-- [0]
flat :: forall v s' s a. (KnownNats s, KnownNats s', s' ~ '[Eval (Size s)], VG.Vector v a) => Array v s a -> Array v s' a
flat a = unsafeModifyShape a

-- | Reshape an array, repeating the original array. The shape of the array should be a suffix of the new shape.
--
-- >>> pretty $ repeat @[2,2,2] (array @'[2] [1,2])
-- [[[1,2],
--   [1,2]],
--  [[1,2],
--   [1,2]]]
--
-- > repeat ds (toScalar x) == konst ds x
repeat :: forall v s' s a. (KnownNats s, KnownNats s', Eval (IsPrefixOf s s') ~ True, VG.Vector v a) => Array v s a -> Array v s' a
repeat a = unsafeBackpermute (List.drop (S.rankL (valuesOf @s') - rank a)) a

-- | Reshape an array, cycling through the elements without regard to the original shape.
--
-- >>> pretty $ cycle @[2,2,2] (array @'[3] [1,2,3])
-- [[[1,2],
--   [3,1]],
--  [[2,3],
--   [1,2]]]
cycle :: forall v s' s a. (KnownNats s, KnownNats s', VG.Vector v a) => Array v s a -> Array v s' a
cycle a = unsafeBackpermute (VU.toList . S.shapen (shape a) . (`mod` size a) . S.flatten (VU.fromList (valuesOf @s')) . VU.fromList) a

-- | Change rank by adding new dimensions at the front, if the new rank is greater, or combining dimensions (from left to right) into rows, if the new rank is lower.
--
-- >>> shape (rerank (SNat @4) a)
-- [1,2,3,4]
-- >>> shape (rerank (SNat @2) a)
-- [6,4]
--
-- > flat == rerank 1
rerank :: forall v r s s' a. (KnownNats s, KnownNats s', s' ~ Eval (Rerank r s), VG.Vector v a) => SNat r -> Array v s a -> Array v s' a
rerank _ a = unsafeModifyShape a

-- | Change the order of dimensions.
--
-- >>> pretty $ reorder (Dims @[2,0,1]) a
-- [[[0,4,8],
--   [12,16,20]],
--  [[1,5,9],
--   [13,17,21]],
--  [[2,6,10],
--   [14,18,22]],
--  [[3,7,11],
--   [15,19,23]]]
reorder :: forall v ds s s' a. (KnownNats s, KnownNats s', s' ~ Eval (Reorder s ds), VG.Vector v a) => SNats ds -> Array v s a -> Array v s' a
reorder SNats a = unsafeBackpermute (\s -> S.insertDimsL (valuesOf @ds) s []) a

-- | Remove single dimensions.
--
-- >>> let sq = array [1..24] :: Array '[2,1,3,4,1] Int
-- >>> shape $ squeeze sq
-- [2,3,4]
--
-- >>> shape $ squeeze (singleton 0)
-- []
squeeze :: forall v s t a. (KnownNats s, KnownNats t, t ~ Eval (Squeeze s), VG.Vector v a) => Array v s a -> Array v t a
squeeze = unsafeModifyShape

-- | Insert a single dimension at the supplied position.
--
-- >>> shape $ elongate (SNat @1) a
-- [2,1,3,4]
-- >>> toDynamic $ elongate (SNat @0) (toScalar 1)
-- UnsafeArray [1] [1]
elongate :: (KnownNats s, KnownNats s', s' ~ Eval (InsertDim d 1 s), VG.Vector v a) => Dim d -> Array v s a -> Array v s' a
elongate _ a = unsafeModifyShape a

-- | Reverse indices eg transposes the element A/ijk/ to A/kji/.
--
-- >>> (transpose a) ! [1,0,0] == a ! [0,0,1]
-- True
-- >>> pretty $ transpose (array @[2,2,2] [1..8])
-- [[[1,5],
--   [3,7]],
--  [[2,6],
--   [4,8]]]
transpose :: forall v a s s'. (KnownNats s, KnownNats s', s' ~ Eval (Reverse s), VG.Vector v a) => Array v s a -> Array v s' a
transpose a = unsafeBackpermute List.reverse a

-- | Inflate (or replicate) an array by inserting a new dimension given a supplied dimension and size.
--
-- >>> pretty $ inflate (SNat @0) (SNat @2) (array @'[3] [0,1,2])
-- [[0,1,2],
--  [0,1,2]]
inflate :: forall v s' s d x a. (KnownNats s, KnownNats s', s' ~ Eval (InsertDim d x s), VG.Vector v a) => Dim d -> SNat x -> Array v s a -> Array v s' a
inflate SNat _ a = unsafeBackpermute (S.deleteDimL (valueOf @d)) a

-- | Intercalate an array along dimensions.
--
-- >>> pretty $ intercalate (SNat @2) (konst @[2,3] 0) a
-- [[[0,0,1,0,2,0,3],
--   [4,0,5,0,6,0,7],
--   [8,0,9,0,10,0,11]],
--  [[12,0,13,0,14,0,15],
--   [16,0,17,0,18,0,19],
--   [20,0,21,0,22,0,23]]]
intercalate :: forall v d ds n n' s si st a. (KnownNats s, KnownNats si, KnownNats st, KnownNats ds, KnownNat n, KnownNat n', ds ~ '[d], si ~ Eval (DeleteDim d s), n ~ Eval (GetDim d s), n' ~ Eval ((Fcf.-) (Eval ((Fcf.+) n n)) 1), st ~ Eval (InsertDim d n' si), VG.Vector v a, VG.Vector v (Array v si a)) => Dim d -> Array v si a -> Array v s a -> Array v st a
intercalate SNat i a =
  joins
    (Dims @ds)
    ( vector @v @n'
        ( List.intersperse
            i
            (toListA (extracts (Dims @ds) a))
        )
    )

-- | Intersperse an element along dimensions.
--
-- >>> pretty $ intersperse (SNat @2) 0 a
-- [[[0,0,1,0,2,0,3],
--   [4,0,5,0,6,0,7],
--   [8,0,9,0,10,0,11]],
--  [[12,0,13,0,14,0,15],
--   [16,0,17,0,18,0,19],
--   [20,0,21,0,22,0,23]]]
intersperse :: forall v d ds n n' s si st a. (KnownNats s, KnownNats si, KnownNats st, KnownNats ds, KnownNat n, KnownNat n', ds ~ '[d], si ~ Eval (DeleteDim d s), n ~ Eval (GetDim d s), n' ~ n + n - 1, st ~ Eval (InsertDim d n' si), VG.Vector v a, VG.Vector v (Array v si a)) => Dim d -> a -> Array v s a -> Array v st a
intersperse (SNat :: SNat d) x a = intercalate (SNat @d) (konst @si x) a

-- | Concatenate dimensions, creating a new dimension at the supplied postion.
--
-- >>> pretty $ concats (Dims @[0,1]) (SNat @1) a
-- [[0,4,8,12,16,20],
--  [1,5,9,13,17,21],
--  [2,6,10,14,18,22],
--  [3,7,11,15,19,23]]
concats :: forall v s s' newd ds a. (KnownNats s, KnownNats s', s' ~ Eval (ConcatDims ds newd s), VG.Vector v a) => Dims ds -> SNat newd -> Array v s a -> Array v s' a
concats SNats SNat a = unsafeBackpermute (S.unconcatDimsIndexL ds n (VU.toList (shape a))) a
  where
    n = valueOf @newd
    ds = valuesOf @ds

-- | Reverses element order along specified dimensions.
--
-- >>> pretty $ reverses (Dims @[0,1]) a
-- [[[20,21,22,23],
--   [16,17,18,19],
--   [12,13,14,15]],
--  [[8,9,10,11],
--   [4,5,6,7],
--   [0,1,2,3]]]
reverses :: forall v ds s a. (KnownNats s, VG.Vector v a) => Dims ds -> Array v s a -> Array v s a
reverses SNats a = unsafeBackpermute (S.reverseIndexL (valuesOf @ds) (VU.toList (shape a))) a

-- | Rotate an array by/along dimensions & offsets.
--
-- >>> pretty $ rotates (Dims @'[1]) [2] a
-- [[[8,9,10,11],
--   [0,1,2,3],
--   [4,5,6,7]],
--  [[20,21,22,23],
--   [12,13,14,15],
--   [16,17,18,19]]]
rotates :: forall v a ds s. (KnownNats s, True ~ Eval (IsDims ds s), VG.Vector v a) => Dims ds -> [Int] -> Array v s a -> Array v s a
rotates SNats rs a = unsafeBackpermute (S.rotatesIndexL (valuesOf @ds) rs (valuesOf @s)) a

-- | Sort an array along the supplied dimensions.
--
-- >>> pretty $ sorts (Dims @'[0]) (array @[2,2] [2,3,1,4])
-- [[1,4],
--  [2,3]]
-- >>> pretty $ sorts (Dims @'[1]) (array @[2,2] [2,3,1,4])
-- [[2,3],
--  [1,4]]
-- >>> pretty $ sorts (Dims @[0,1]) (array @[2,2] [2,3,1,4])
-- [[1,2],
--  [3,4]]
sorts :: forall v ds s a si so. (Ord a, KnownNats s, KnownNats si, KnownNats so, si ~ Eval (DeleteDims ds s), so ~ Eval (GetDims ds s), s ~ Eval (InsertDims ds so si), VG.Vector v a, Ord (v a), VG.Vector v (Array v si a)) => Dims ds -> Array v s a -> Array v s a
sorts SNats a = joins (Dims @ds) $ unsafeModifyVector (VG.convert . sortV . VG.convert) (extracts (Dims @ds) a)

-- | The indices into the array if it were sorted by a comparison function along the dimensions supplied.
--
-- >>> import Data.Ord (Down (..))
-- >>> toDynamic $ sortsBy (Dims @'[0]) (fmap Down) (array @[2,2] [2,3,1,4])
-- UnsafeArray [2,2] [2,3,1,4]
sortsBy :: forall v ds s a b si so. (Ord b, KnownNats s, KnownNats si, KnownNats so, si ~ Eval (DeleteDims ds s), so ~ Eval (GetDims ds s), s ~ Eval (InsertDims ds so si), VG.Vector v a, VG.Vector v (Array v si a), Ord (v b)) => Dims ds -> (Array v si a -> Array v si b) -> Array v s a -> Array v s a
sortsBy SNats c a = joins (Dims @ds) $ unsafeModifyVector (VG.convert . sortByV c . VG.convert) (extracts (Dims @ds) a)

-- | The indices into the array if it were sorted along the dimensions supplied.
--
-- >>> orders (Dims @'[0]) (array @[2,2] [2,3,1,4])
-- [1,0]
orders :: forall v ds s a si so. (Ord a, KnownNats s, KnownNats si, KnownNats so, si ~ Eval (DeleteDims ds s), so ~ Eval (GetDims ds s), s ~ Eval (InsertDims ds so si), VG.Vector v a, VG.Vector v Int, Ord (v a), VG.Vector v (Array v si a)) => Dims ds -> Array v s a -> Array v so Int
orders SNats a = unsafeModifyVector (VG.convert . orderV . VG.convert) (extracts (Dims @ds) a)

-- | The indices into the array if it were sorted by a comparison function along the dimensions supplied.
--
-- >>> import Data.Ord (Down (..))
-- >>> ordersBy (Dims @'[0]) (fmap Down) (array @[2,2] [2,3,1,4])
-- [0,1]
ordersBy :: forall v ds s a b si so. (Ord b, KnownNats s, KnownNats si, KnownNats so, si ~ Eval (DeleteDims ds s), so ~ Eval (GetDims ds s), s ~ Eval (InsertDims ds so si), VG.Vector v a, VG.Vector v Int, VG.Vector v (Array v si a), Ord (v b)) => Dims ds -> (Array v si a -> Array v si b) -> Array v s a -> Array v so Int
ordersBy SNats c a = unsafeModifyVector (VG.convert . orderByV c . VG.convert) (extracts (Dims @ds) a)

-- | Apply a binary array function to two arrays with matching shapes across the supplied (matching) dimensions.
--
-- >>> a = array @[2,3] [0..5]
-- >>> b = array @'[3] [6..8]
-- >>> pretty $ telecasts (Dims @'[1]) (Dims @'[0]) (concatenate (SNat @0)) a b
-- [[0,3,6],
--  [1,4,7],
--  [2,5,8]]
telecasts :: forall v sa sb sc sia sib sic ma mb a b c soa sob ds. (KnownNats sa, KnownNats sb, KnownNats sc, KnownNats sia, KnownNats sib, KnownNats sic, KnownNats soa, KnownNats sob, KnownNats ds, ds ~ Eval (DimsOf soa), sia ~ Eval (DeleteDims ma sa), sib ~ Eval (DeleteDims mb sb), soa ~ Eval (GetDims ma sa), sob ~ Eval (GetDims mb sb), soa ~ sob, sc ~ Eval (InsertDims ds soa sic), VG.Vector v a, VG.Vector v b, VG.Vector v c, VG.Vector v (Array v sia a), VG.Vector v (Array v sib b), VG.Vector v (Array v sic c)) => SNats ma -> SNats mb -> (Array v sia a -> Array v sib b -> Array v sic c) -> Array v sa a -> Array v sb b -> Array v sc c
telecasts SNats SNats f a b = join (zipWith f (extracts (SNats @ma) a) (extracts (SNats @mb) b))

-- | Apply a binary array function to two arrays where the shape of the first array is a prefix of the second array.
--
-- >>> a = array @[2,3] [0..5]
-- >>> pretty $ transmit (zipWith (+)) (toScalar 1) a
-- [[1,2,3],
--  [4,5,6]]
transmit :: forall v sa sb sc a b c ds sib sic sob. (KnownNats sa, KnownNats sb, KnownNats sc, KnownNats ds, KnownNats sib, KnownNats sic, KnownNats sob, ds ~ Eval (EnumFromTo (Eval (Rank sa)) (Eval (Rank sb) - 1)), sib ~ Eval (DeleteDims ds sb), sob ~ Eval (GetDims ds sb), sb ~ Eval (InsertDims ds sob sib), sc ~ Eval (InsertDims ds sob sic), True ~ Eval (IsPrefixOf sa sb), VG.Vector v a, VG.Vector v b, VG.Vector v c, VG.Vector v (Array v sib b), VG.Vector v (Array v sic c)) => (Array v sa a -> Array v sib b -> Array v sic c) -> Array v sa a -> Array v sb b -> Array v sc c
transmit f a b = maps (Dims @ds) (f a) b

-- | A one-dimensional array.
type Vector v s a = Array v '[s] a

-- | Create a one-dimensional array.
--
-- >>> pretty $ vector @3 @Int [2,3,4]
-- [2,3,4]
vector :: forall v n a t. (FromVector t v a, KnownNat n, VG.Vector v a) => t -> Array v '[n] a
vector xs = array xs

-- | vector with an explicit SNat rather than a KnownNat constraint.
--
-- >>> pretty $ vector' @Int (SNat @3) [2,3,4]
-- [2,3,4]
vector' :: forall v a n t. (FromVector t v a, VG.Vector v a) => SNat n -> t -> Array v '[n] a
vector' n xs = withKnownNat n (vector xs)

-- | Vector specialisation of 'range'
--
-- >>> toDynamic $ iota @5
-- UnsafeArray [5] [0,1,2,3,4]
iota :: forall v n. (KnownNat n, VG.Vector v Int) => Vector v n Int
iota = range

-- | A two-dimensional array.
type Matrix v m n a = Array v '[m, n] a

-- * row (first dimension) specializations

-- | Add a new row
--
-- >>> pretty $ cons (array @'[2] [0,1]) (array @[2,2] [2,3,4,5])
-- [[0,1],
--  [2,3],
--  [4,5]]
cons :: forall v st s sh a. (KnownNats st, KnownNats s, KnownNats sh, True ~ Eval (InsertOk 0 st sh), s ~ Eval (IncAt 0 st), sh ~ Eval (DeleteDim 0 st), VG.Vector v a) => Array v sh a -> Array v st a -> Array v s a
cons =
  prepend (SNat @0)

-- | Add a new row at the end
--
-- >>> pretty $ snoc (array @[2,2] [0,1,2,3]) (array @'[2] [4,5])
-- [[0,1],
--  [2,3],
--  [4,5]]
snoc :: forall v si s sl a. (KnownNats si, KnownNats s, KnownNats sl, True ~ Eval (InsertOk 0 si sl), s ~ Eval (IncAt 0 si), sl ~ Eval (DeleteDim 0 si), VG.Vector v a) => Array v si a -> Array v sl a -> Array v s a
snoc = append (SNat @0)

-- | split an array into the first row and the remaining rows.
--
-- >>> import Data.Bifunctor (bimap)
-- >>> bimap toDynamic toDynamic $ uncons (array @[3,2] [0..5])
-- (UnsafeArray [2] [0,1],UnsafeArray [2,2] [2,3,4,5])
uncons :: forall v a s sh st ls os ds. (KnownNats s, KnownNats sh, KnownNats st, ds ~ '[0], sh ~ Eval (DeleteDims ds s), KnownNats ls, KnownNats os, os ~ Eval (Replicate (Eval (Rank ds)) 1), ls ~ Eval (GetLastPositions ds s), Eval (SlicesOk ds os ls s) ~ True, st ~ Eval (SetDims ds ls s), VG.Vector v a) => Array v s a -> (Array v sh a, Array v st a)
uncons a = (heads (Dims @ds) a, tails (Dims @ds) a)

-- | split an array into the initial rows and the last row.
--
-- >>> import Data.Bifunctor (bimap)
-- >>> bimap toDynamic toDynamic $ unsnoc (array @[3,2] [0..5])
-- (UnsafeArray [2,2] [0,1,2,3],UnsafeArray [2] [4,5])
unsnoc :: forall v ds os s a ls si sl. (KnownNats s, KnownNats ds, KnownNats si, KnownNats ls, KnownNats os, KnownNats sl, ds ~ '[0], Eval (SlicesOk ds os ls s) ~ True, os ~ Eval (Replicate (Eval (Rank ds)) 0), ls ~ Eval (GetLastPositions ds s), si ~ Eval (SetDims ds ls s), sl ~ Eval (DeleteDims ds s), VG.Vector v a) => Array v s a -> (Array v si a, Array v sl a)
unsnoc a = (inits (Dims @ds) a, lasts (Dims @ds) a)

-- | Convenience pattern for row extraction and consolidation at the beginning of an Array.
--
-- >>> (x:<xs) = array @'[4] [0..3]
-- >>> toDynamic x
-- UnsafeArray [] [0]
-- >>> toDynamic xs
-- UnsafeArray [3] [1,2,3]
-- >>> toDynamic (x:<xs)
-- UnsafeArray [4] [0,1,2,3]
pattern (:<) :: forall v s sh st a os ls ds. (KnownNats s, KnownNats sh, KnownNats st, True ~ Eval (InsertOk 0 st sh), s ~ Eval (IncAt 0 st), ds ~ '[0], sh ~ Eval (DeleteDims ds s), KnownNats ls, KnownNats os, Eval (SlicesOk ds os ls s) ~ True, os ~ Eval (Replicate (Eval (Rank ds)) 1), ls ~ Eval (GetLastPositions ds s), st ~ Eval (SetDims ds ls s), VG.Vector v a) => Array v sh a -> Array v st a -> Array v s a
pattern x :< xs <- (uncons -> (x, xs))
  where
    x :< xs = cons x xs

infix 5 :<

{-# COMPLETE (:<) :: Array #-}

-- | Convenience pattern for row extraction and consolidation at the end of an Array.
--
-- >>> (xs:>x) = array @'[4] [0..3]
-- >>> toDynamic x
-- UnsafeArray [] [3]
-- >>> toDynamic xs
-- UnsafeArray [3] [0,1,2]
-- >>> toDynamic (xs:>x)
-- UnsafeArray [4] [0,1,2,3]
pattern (:>) :: forall v si sl s a ds ls os. (KnownNats si, KnownNats sl, KnownNats s, True ~ Eval (InsertOk 0 si sl), s ~ Eval (IncAt 0 si), KnownNats ds, KnownNats ls, KnownNats os, sl ~ Eval (DeleteDim 0 si), ds ~ '[0], Eval (SlicesOk ds os ls s) ~ True, os ~ Eval (Replicate (Eval (Rank ds)) 0), ls ~ Eval (GetLastPositions ds s), si ~ Eval (SetDims ds ls s), sl ~ Eval (DeleteDims ds s), VG.Vector v a) => Array v si a -> Array v sl a -> Array v s a
pattern xs :> x <- (unsnoc -> (xs, x))
  where
    xs :> x = snoc xs x

infix 5 :>

{-# COMPLETE (:>) :: Array #-}

-- | Generate an array of uniform random variates between a range.
--
-- >>> import System.Random.Stateful hiding (uniform)
-- >>> g <- newIOGenM (mkStdGen 42)
-- >>> u <- uniform @[2,3,4] @Int g (0,9)
-- >>> pretty u
-- [[[0,7,0,2],
--   [1,7,4,2],
--   [5,9,8,2]],
--  [[9,8,1,0],
--   [2,2,8,2],
--   [2,8,0,6]]]
uniform :: forall v s a g m. (StatefulGen g m, UniformRange a, KnownNats s, VG.Vector v a) => g -> (a, a) -> m (Array v s a)
uniform g r = do
  v <- VG.replicateM (S.size (VU.fromList (valuesOf @s))) (uniformRM r g) :: m (v a)
  pure $ array @v @s v


