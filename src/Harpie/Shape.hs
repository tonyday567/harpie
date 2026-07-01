{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | Functions for manipulating shape. The module tends to supply equivalent functionality at type-level and value-level with functions of the same name (except for capitalization).
module Harpie.Shape
  ( -- * Type-level Nat
    SNat,
    pattern SNat,
    valueOf,

    -- * Type-level [Nat]
    SNats,
    pattern SNats,
    fromSNats,
    KnownNats (..),
    natVals,
    withKnownNats,
    SomeNats,
    someNatVals,
    withSomeSNats,

    -- * Shape
    valuesOf,
    rankOf,
    sizeOf,
    Fin (..),
    fin,
    safeFin,
    Fins (..),
    fins,
    safeFins,

    -- * Shape Operators at value- and type- level.
    rank,
    rankL,
    Rank,
    range,
    rangeL,
    Range,
    rerank,
    rerankL,
    Rerank,
    dimsOf,
    dimsOfL,
    DimsOf,
    endDimsOf,
    endDimsOfL,
    EndDimsOf,
    size,
    sizeL,
    Size,
    flatten,
    flattenL,
    flattenStrides,
    flattenStridesL,
    shapen,
    shapenL,
    stridesOf,
    stridesOfL,
    shapenStrides,
    shapenStridesL,
    asSingleton,
    asSingletonL,
    AsSingleton,
    asScalar,
    asScalarL,
    AsScalar,
    lte,
    lteL,
    LTE,
    isSubset,
    isSubsetL,
    IsSubset,
    exceptDims,
    exceptDimsL,
    ExceptDims,
    reorder,
    reorderL,
    Reorder,
    ReorderOk,
    squeeze,
    squeezeL,
    Squeeze,

    -- * Primitives
    Min,
    Max,
    minimum,
    minimumL,
    Minimum,

    -- * Position
    isFin,
    IsFin,
    isFins,
    isFinsL,
    IsFins,
    isDim,
    isDimL,
    IsDim,
    isDims,
    isDimsL,
    IsDims,
    lastPos,
    lastPosL,
    LastPos,
    minDim,
    minDimL,
    MinDim,

    -- * combinators
    EnumFromTo,
    Foldl',

    -- * single dimension
    GetIndex,
    getDim,
    getDimL,
    GetDim,
    modifyDim,
    modifyDimL,
    ModifyDim,
    incAt,
    incAtL,
    IncAt,
    decAt,
    decAtL,
    DecAt,
    setDim,
    setDimL,
    SetDim,
    takeDim,
    takeDimL,
    TakeDim,
    halfDim,
    Half,
    HalveDim,
    dropDim,
    dropDimL,
    DropDim,
    deleteDim,
    deleteDimL,
    DeleteDim,
    insertDim,
    insertDimL,
    InsertDim,
    InsertOk,
    SliceOk,
    SlicesOk,
    concatenate,
    concatenateL,
    Concatenate,
    ConcatenateOk,

    -- * multiple dimension
    getDims,
    getDimsL,
    GetDims,
    getLastPositions,
    getLastPositionsL,
    GetLastPositions,
    modifyDims,
    modifyDimsL,
    insertDims,
    insertDimsL,
    InsertDims,
    preDeletePositions,
    preDeletePositionsL,
    PreDeletePositions,
    preInsertPositions,
    preInsertPositionsL,
    PreInsertPositions,
    setDims,
    setDimsL,
    SetDims,
    deleteDims,
    deleteDimsL,
    DeleteDims,
    dropDims,
    dropDimsL,
    DropDims,
    concatDims,
    concatDimsL,
    ConcatDims,

    -- * value-only operations
    unconcatDimsIndex,
    unconcatDimsIndexL,
    reverseIndex,
    reverseIndexL,
    rotate,
    rotateL,
    rotateIndex,
    rotateIndexL,
    rotatesIndex,
    rotatesIndexL,
    isDiag,
    isDiagL,

    -- * windowed
    expandWindows,
    expandWindowsL,
    ExpandWindows,
    indexWindows,
    indexWindowsL,
    dimWindows,
    dimWindowsL,
    DimWindows,

    -- * Fcf re-exports
    Eval,
    type (++),
  )
where

import Data.Bool
import Data.Foldable hiding (minimum)
import Data.Function
import Data.List qualified as List
import Data.Maybe
import Data.Proxy
import Data.Type.Bool hiding (Not)
import Data.Type.Equality
import Data.Type.Ord hiding (Max, Min)
import Data.Vector.Unboxed qualified as VU
import Fcf hiding (type (&&), type (+), type (++), type (-), type (<), type (>), type (||))
import Fcf qualified
import Fcf.Class.Foldable
import Fcf.Data.List
import GHC.Exts
import GHC.TypeLits (ErrorMessage (..))
import GHC.TypeLits qualified as L
import GHC.TypeNats
import Prelude as P hiding (minimum)

{-# ANN module ("doctest-parallel: --no-implicit-module-import" :: String) #-}

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeFamilies
-- >>> import Prelude
-- >>> import Fcf
-- >>> import GHC.Exts ()
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> import Harpie.Shape as S

-- | Get the value of a type level Nat.
-- Use with explicit type application
--
-- >>> valueOf @42
-- 42
valueOf :: forall n. (KnownNat n) => Int
valueOf = fromIntegral $ fromSNat (SNat @n)
{-# INLINE valueOf #-}

type role SNats nominal

-- | A value-level witness for a type-level list of natural numbers.
--
-- Obtain an SNats value using:
--
-- - The natsSing method of KnownNats
-- - The SNats pattern
-- - The withSomeSNats function
--
-- >>> :t SNats @[2,3,4]
-- SNats @[2,3,4] :: KnownNats [2, 3, 4] => SNats [2, 3, 4]
-- >>> SNats @[2,3,4]
-- SNats @[2, 3, 4]
newtype SNats (ns :: [Nat]) = UnsafeSNats [Nat]

instance Eq (SNats ns) where
  _ == _ = True

instance Ord (SNats ns) where
  compare _ _ = EQ

-- | Matches GHC printing quirks.
instance Show (SNats ns) where
  show (UnsafeSNats s) = "SNats @" <> bool "" "'" (length s < 2) <> "[" <> mconcat (List.intersperse ", " (show <$> s)) <> "]"

-- | A explicitly bidirectional pattern synonym relating an 'SNats' to a 'KnownNats' constraint.
--
-- As an expression: Constructs an explicit 'SNats' ns value from an implicit 'KnownNats' ns constraint:
--
-- > SNat @n :: KnownNat n => SNat n
--
-- As a pattern: Matches on an explicit SNats n value bringing an implicit KnownNats n constraint into scope:
--
-- > f :: SNats ns -> ..
-- > f SNat = {- KnownNats ns in scope -}
--
-- or, if you need to both bring the KnownNats into scope and reuse the SNats input:
--
-- > f (SNats :: SNats s) = g (SNats @s)
pattern SNats :: forall ns. () => (KnownNats ns) => SNats ns
pattern SNats <- (knownNatsInstance -> KnownNatsInstance)
  where
    SNats = natsSing

{-# COMPLETE SNats #-}

-- | Return the value-level list of naturals in an SNats ns value.
--
-- >>> fromSNats (SNats @[2,3,4])
-- [2,3,4]
fromSNats :: SNats s -> [Nat]
fromSNats (UnsafeSNats s) = s

-- An internal data type that is only used for defining the SNat pattern
-- synonym.
data KnownNatsInstance (ns :: [Nat]) where
  KnownNatsInstance :: (KnownNats ns) => KnownNatsInstance ns

-- An internal function that is only used for defining the SNat pattern
-- synonym.
knownNatsInstance :: SNats ns -> KnownNatsInstance ns
knownNatsInstance dims = withKnownNats dims KnownNatsInstance

-- | Reflect a list of naturals.
--
-- >>> natsSing @'[2]
-- SNats @'[2]
class KnownNats (ns :: [Nat]) where
  natsSing :: SNats ns

instance KnownNats '[] where
  natsSing = UnsafeSNats []

instance (KnownNat n, KnownNats s) => KnownNats (n ': s) where
  natsSing = UnsafeSNats (fromSNat (SNat :: SNat n) : fromSNats (SNats :: SNats s))

-- | Obtain a value-level list of naturals from a type-level proxy
--
-- >>> natVals (SNats @[2,3,4])
-- [2,3,4]
natVals :: forall ns proxy. (KnownNats ns) => proxy ns -> [Nat]
natVals _ = case natsSing :: SNats ns of
  UnsafeSNats xs -> xs

-- | Convert an explicit SNats ns value into an implicit KnownNats ns constraint.
withKnownNats ::
  forall ns rep (r :: TYPE rep).
  SNats ns -> ((KnownNats ns) => r) -> r
withKnownNats = withDict @(KnownNats ns)

-- | Convert a list of naturals into an SNats ns value, where ns is a fresh type-level list of naturals.
withSomeSNats ::
  forall rep (r :: TYPE rep).
  [Nat] -> (forall s. SNats s -> r) -> r
withSomeSNats s k = k (UnsafeSNats s)
{-# NOINLINE withSomeSNats #-}

-- | An unknown type-level list of naturals.
data SomeNats = forall s. (KnownNats s) => SomeNats (Proxy s)

-- | Promote a list of naturals to unknown type-level
someNatVals :: [Nat] -> SomeNats
someNatVals s =
  withSomeSNats
    s
    ( \(sn :: SNats s) ->
        withKnownNats sn (SomeNats @s Proxy)
    )

-- * shape primitives

-- | The value of a 'KnownNats'.
--
-- >>> valuesOf @[2,3,4]
-- [2,3,4]
valuesOf :: forall s. (KnownNats s) => [Int]
valuesOf = fmap fromIntegral (fromSNats (SNats :: SNats s))
{-# INLINE valuesOf #-}

-- | The rank (or length) of a KnownNats.
--
-- >>> rankOf @[2,3,4]
-- 3
rankOf :: forall s. (KnownNats s) => Int
rankOf = length (valuesOf @s)
{-# INLINE rankOf #-}

-- | The size (or product) of a KnownNats.
--
-- >>> sizeOf @[2,3,4]
-- 24
sizeOf :: forall s. (KnownNats s) => Int
sizeOf = product (valuesOf @s)
{-# INLINE sizeOf #-}

-- | Fin most often represents a (finite) zero-based index for a single dimension (of a multi-dimensioned hyper-rectangular array).
type role Fin nominal

newtype Fin s
  = UnsafeFin
  { fromFin :: Int
  }
  deriving stock (Eq, Ord)

instance Show (Fin n) where
  show (UnsafeFin x) = show x

-- | Construct a Fin.
--
-- Errors on out-of-bounds
--
-- >>> fin @2 1
-- 1
--
-- >>> fin @2 2
-- *** Exception: value outside bounds
-- ...
fin :: forall n. (KnownNat n) => Int -> Fin n
fin x = fromMaybe (error "value outside bounds") (safeFin x)

-- | Construct a Fin safely.
--
-- >>> safeFin 1 :: Maybe (Fin 2)
-- Just 1
--
-- >>> safeFin 2 :: Maybe (Fin 2)
-- Nothing
safeFin :: forall n. (KnownNat n) => Int -> Maybe (Fin n)
safeFin x = bool Nothing (Just (UnsafeFin x)) (x >= 0 && x < valueOf @n)

-- | Fins most often represents (finite) indexes for multiple dimensions (of a multi-dimensioned hyper-rectangular array).
type role Fins nominal

newtype Fins s
  = UnsafeFins
  { fromFins :: [Int]
  }
  deriving stock (Eq, Ord, Functor)

instance Show (Fins n) where
  show (UnsafeFins x) = show x

-- | Construct a Fins.
--
-- Errors on out-of-bounds
--
-- >>> fins @[2,3,4] [1,2,3]
-- [1,2,3]
--
-- >>> fins @[2,3,4] [1,2,5]
-- *** Exception: value outside bounds
-- ...
fins :: forall s. (KnownNats s) => [Int] -> Fins s
fins x = fromMaybe (error "value outside bounds") (safeFins x)

-- | Construct a Fins safely.
--
-- >>> safeFins [1,2,3] :: Maybe (Fins [2,3,4])
-- Just [1,2,3]
--
-- >>> safeFins [2] :: Maybe (Fins '[2])
-- Nothing
safeFins :: forall s. (KnownNats s) => [Int] -> Maybe (Fins s)
safeFins xs = bool Nothing (Just (UnsafeFins xs)) (isFinsL xs (valuesOf @s))

-- | Number of dimensions
--
-- >>> rank (VU.fromList [2,3,4])
-- 3
rank :: VU.Vector Int -> Int
rank = VU.length
{-# INLINE rank #-}

-- | Number of dimensions
--
-- >>> rankL @Int [2,3,4]
-- 3
rankL :: [a] -> Int
rankL = length
{-# INLINE rankL #-}

-- | Number of dimensions
--
-- >>> :k! Eval (Rank [2,3,4])
-- ...
-- = 3
data Rank :: [a] -> Exp Natural

type instance
  Eval (Rank xs) =
    Eval (Length xs)

-- | Enumerate a range of rank n
--
-- >>> range 0
-- []
--
-- >>> range 3
-- [0,1,2]
range :: Int -> VU.Vector Int
range n = VU.enumFromTo 0 (n - 1)

-- | Enumerate a range of rank n
--
-- >>> rangeL 0
-- []
--
-- >>> rangeL 3
-- [0,1,2]
rangeL :: Int -> [Int]
rangeL n = [0 .. (n - 1)]

-- | Enumerate between two Nats
--
-- >>> :k! Eval (EnumFromTo 0 3)
-- ...
-- = [0, 1, 2, 3]
data EnumFromTo :: Nat -> Nat -> Exp [Nat]

type instance Eval (EnumFromTo a b) = Eval (Unfoldr (EnumFromToHelper b) a)

data EnumFromToHelper :: Nat -> Nat -> Exp (Maybe (a, Nat))

type instance
  Eval (EnumFromToHelper b a) =
    If
      (a >? b)
      'Nothing
      ('Just '(a, a + 1))

-- | Enumerate a range of rank n
--
-- >>> :k! Eval (Range 0)
-- ...
-- = '[]
--
-- >>> :k! Eval (Range 3)
-- ...
-- = [0, 1, 2]
data Range :: Nat -> Exp [Nat]

type instance
  Eval (Range x) =
    If (x == 0) '[] (Eval (EnumFromTo 0 (Eval ((Fcf.-) x 1))))

-- | Create a new rank by adding ones to the left, if the new rank is greater, or combining dimensions (from left to right) into rows, if the new rank is lower.
--
-- >>> rerank 4 (VU.fromList [2,3,4])
-- [1,2,3,4]
-- >>> rerank 2 (VU.fromList [2,3,4])
-- [6,4]
rerank :: Int -> VU.Vector Int -> VU.Vector Int
rerank r xs =
  VU.replicate (r - r') 1
    VU.++ bool VU.empty (VU.singleton (VU.product (VU.take (r' - r + 1) xs))) (r <= r')
    VU.++ VU.drop (r' - r + 1) xs
  where
    r' = rank xs

-- | Create a new rank by adding ones to the left, if the new rank is greater, or combining dimensions (from left to right) into rows, if the new rank is lower.
--
-- >>> rerankL 4 [2,3,4]
-- [1,2,3,4]
-- >>> rerankL 2 [2,3,4]
-- [6,4]
rerankL :: Int -> [Int] -> [Int]
rerankL r xs =
  replicate (r - r') 1
    <> bool [] [product (take (r' - r + 1) xs)] (r <= r')
    <> drop (r' - r + 1) xs
  where
    r' = rankL xs

-- | Create a new rank by adding ones to the left, if the new rank is greater, or combining dimensions (from left to right) into rows, if the new rank is lower.
--
-- >>> :k! Eval (Rerank 4 [2,3,4])
-- ...
-- = [1, 2, 3, 4]
-- >>> :k! Eval (Rerank 2 [2,3,4])
-- ...
-- = [6, 4]
data Rerank :: Nat -> [Nat] -> Exp [Nat]

type instance
  Eval (Rerank r xs) =
    If
      (Eval ((Fcf.>) r (Eval (Rank xs))))
      (Eval (Eval (Replicate (Eval ((Fcf.-) r (Eval (Rank xs)))) 1) ++ xs))
      ( Eval
          ( '[Eval (Size (Eval (Take (Eval ((Fcf.+) (Eval ((Fcf.-) (Eval (Rank xs)) r)) 1)) xs)))]
              ++ Eval (Drop (Eval ((Fcf.+) (Eval ((Fcf.-) (Eval (Rank xs)) r)) 1)) xs)
          )
      )

-- | Enumerate the dimensions of a shape.
--
-- dimsOf (VU.fromList [2,3,4])
-- [0,1,2]
dimsOf :: VU.Vector Int -> VU.Vector Int
dimsOf s = range (rank s)

-- | Enumerate the dimensions of a shape.
--
-- dimsOfL [2,3,4]
-- [0,1,2]
dimsOfL :: [Int] -> [Int]
dimsOfL s = rangeL (rankL s)

-- | Enumerate the dimensions of a shape.
--
-- >>> :k! Eval (DimsOf [2,3,4])
-- ...
-- = [0, 1, 2]
data DimsOf :: [Nat] -> Exp [Nat]

type instance
  Eval (DimsOf xs) =
    Eval (Range =<< Rank xs)

-- | Enumerate the final dimensions of a shape.
--
-- >>> endDimsOf (VU.fromList [1,0]) (VU.fromList [2,3,4])
-- [2,1]
endDimsOf :: VU.Vector Int -> VU.Vector Int -> VU.Vector Int
endDimsOf xs s = VU.take (rank xs) (VU.reverse (dimsOf s))

-- | Enumerate the final dimensions of a shape.
--
-- >>> endDimsOfL [1,0] [2,3,4]
-- [2,1]
endDimsOfL :: [Int] -> [Int] -> [Int]
endDimsOfL xs s = take (rankL xs) (List.reverse (dimsOfL s))

-- | Enumerate the final dimensions of a shape.
--
-- >>> :k! Eval (EndDimsOf [1,0] [2,3,4])
-- ...
-- = [2, 1]
data EndDimsOf :: [Nat] -> [Nat] -> Exp [Nat]

type instance
  Eval (EndDimsOf xs s) =
    Eval (LiftM2 Take (Rank xs) (Reverse =<< DimsOf s))

-- | Total number of elements (if the list is the shape of a hyper-rectangular array).
--
-- >>> size (VU.fromList [2,3,4])
-- 24
size :: VU.Vector Int -> Int
size v
  | VU.null v = 1
  | VU.length v == 1 = VU.head v
  | otherwise = VU.product v
{-# INLINE size #-}

-- | Total number of elements (if the list is the shape of a hyper-rectangular array).
--
-- >>> sizeL [2,3,4]
-- 24
sizeL :: [Int] -> Int
sizeL [] = 1
sizeL [x] = x
sizeL xs = P.product xs
{-# INLINE sizeL #-}

-- | Total number of elements (if the list is the shape of a hyper-rectangular array).
--
-- >>> :k! (Eval (Size [2,3,4]))
-- ...
-- = 24
data Size :: [Nat] -> Exp Nat

type instance Eval (Size xs) = Eval (Foldr (Fcf.*) 1 xs)

-- | Convert from a n-dimensional shape list index to a flat index, which, technically is the lexicographic position of the position in a row-major array.
--
-- >>> flatten (VU.fromList [2,3,4]) (VU.fromList [1,1,1])
-- 17
--
-- >>> flatten (VU.fromList []) (VU.fromList [1,1,1])
-- 0
flatten :: VU.Vector Int -> VU.Vector Int -> Int
flatten ns _ | VU.null ns = 0
flatten _ xs | VU.length xs == 1 = VU.head xs
flatten ns xs = VU.sum $ VU.zipWith (*) xs (VU.drop 1 $ VU.scanr (*) 1 ns)
{-# INLINE flatten #-}

-- | Convert from a n-dimensional shape list index to a flat index.
--
-- >>> flattenL [2,3,4] [1,1,1]
-- 17
--
-- >>> flattenL [] [1,1,1]
-- 0
flattenL :: [Int] -> [Int] -> Int
flattenL [] _ = 0
flattenL _ [x] = x
flattenL ns xs = sum $ zipWith (*) xs (stridesOfL ns)
{-# INLINE flattenL #-}

-- | Convert from an n-dimensional index to a flat index using precomputed strides.
--
-- >>> flattenStrides (VU.fromList [3,1]) (VU.fromList [1,1])
-- 4
flattenStrides :: VU.Vector Int -> VU.Vector Int -> Int
flattenStrides strides idx = go 0 0
  where
    n = min (VU.length idx) (VU.length strides)
    go acc k
      | k == n = acc
      | otherwise = go (acc + VU.unsafeIndex idx k * VU.unsafeIndex strides k) (k + 1)
{-# INLINE flattenStrides #-}

-- | Convert from an n-dimensional index to a flat index using precomputed strides.
flattenStridesL :: [Int] -> [Int] -> Int
flattenStridesL strides idx = go 0 idx strides
  where
    go acc (x : xs) (s : ss) = go (acc + x * s) xs ss
    go acc _ _ = acc
{-# INLINE flattenStridesL #-}

-- | Convert from a flat index to a shape index.
--
-- >>> shapen (VU.fromList [2,3,4]) 17
-- [1,1,1]
shapen :: VU.Vector Int -> Int -> VU.Vector Int
shapen ss x = shapenStrides (stridesOf ss) x
{-# INLINE shapen #-}

-- | Convert from a flat index to a shape index.
--
-- >>> shapenL [2,3,4] 17
-- [1,1,1]
shapenL :: [Int] -> Int -> [Int]
shapenL ss x = shapenStridesL (stridesOfL ss) x

-- | Precompute strides from shape.
stridesOf :: VU.Vector Int -> VU.Vector Int
stridesOf = VU.drop 1 . VU.scanr (*) 1

-- | Internal: shapen with precomputed strides, avoiding repeated scanr.
shapenStrides :: VU.Vector Int -> Int -> VU.Vector Int
shapenStrides v _ | VU.null v = VU.empty
shapenStrides v x' | VU.length v == 1 = VU.singleton x'
shapenStrides v r =
  let s = VU.head v
      ss' = VU.tail v
      (i, j) = divMod r s
   in VU.cons i (shapenStrides ss' j)
{-# INLINE shapenStrides #-}

-- | Precompute strides from shape.
stridesOfL :: [Int] -> [Int]
stridesOfL = drop 1 . scanr (*) 1

-- | Internal: shapen with precomputed strides, avoiding repeated scanr.
shapenStridesL :: [Int] -> Int -> [Int]
shapenStridesL [] _ = []
shapenStridesL [_] x' = [x']
shapenStridesL (s : ss') r = let (i, j) = divMod r s in i : shapenStridesL ss' j
{-# INLINE shapenStridesL #-}

-- | Convert a scalar to a dimensioned shape
--
-- >>> asSingleton (VU.fromList [])
-- [1]
-- >>> asSingleton (VU.fromList [2,3,4])
-- [2,3,4]
asSingleton :: VU.Vector Int -> VU.Vector Int
asSingleton v
  | VU.null v = VU.singleton 1
  | otherwise = v

-- | Convert a scalar to a dimensioned shape
--
-- >>> asSingletonL []
-- [1]
-- >>> asSingletonL [2,3,4]
-- [2,3,4]
asSingletonL :: [Int] -> [Int]
asSingletonL [] = [1]
asSingletonL x = x

-- | Convert a scalar to a dimensioned shape
-- >>> :k! Eval (AsSingleton '[])
-- ...
-- = '[1]
-- >>> :k! Eval (AsSingleton [2,3,4])
-- ...
-- = [2, 3, 4]
data AsSingleton :: [Nat] -> Exp [Nat]

type instance
  Eval (AsSingleton xs) =
    If (xs == '[]) '[1] xs

-- | Convert a (potentially) [1] dimensioned shape to a scalar shape
--
-- >>> asScalar (VU.fromList [1])
-- []
-- >>> asScalar (VU.fromList [2,3,4])
-- [2,3,4]
asScalar :: VU.Vector Int -> VU.Vector Int
asScalar v
  | VU.length v == 1 && VU.head v == 1 = VU.empty
  | otherwise = v

-- | Convert a (potentially) [1] dimensioned shape to a scalar shape
--
-- >>> asScalarL [1]
-- []
-- >>> asScalarL [2,3,4]
-- [2,3,4]
asScalarL :: [Int] -> [Int]
asScalarL [1] = []
asScalarL x = x

-- | Convert a (potentially) [1] dimensioned shape to a scalar shape
-- >>> :k! Eval (AsScalar '[1])
-- ...
-- = '[]
-- >>> :k! Eval (AsScalar [2,3,4])
-- ...
-- = [2, 3, 4]
data AsScalar :: [Nat] -> Exp [Nat]

type instance
  Eval (AsScalar xs) =
    If (xs == '[1]) '[] xs

-- | Check if a shape is a subset (<=) another shape after reranking.
--
-- >>> lte (VU.fromList [2,3,4]) (VU.fromList [2,3,4])
-- True
--
-- >>> lte (VU.fromList [1,2]) (VU.fromList [2,3,4])
-- True
--
-- >>> lte (VU.fromList [2,1]) (VU.fromList [1])
-- False
lte :: VU.Vector Int -> VU.Vector Int -> Bool
lte xs ys =
  VU.and (VU.zipWith (<=) (rerank (rank ys) xs) ys)

-- | Check if a shape is a subset (<=) another shape after reranking.
--
-- >>> lteL [2,3,4] [2,3,4]
-- True
--
-- >>> lteL [1,2] [2,3,4]
-- True
--
-- >>> lteL [2,1] [1]
-- False
lteL :: [Int] -> [Int] -> Bool
lteL xs ys =
  and (zipWith (<=) (rerankL (rankL ys) xs) ys)

data LTE :: [Nat] -> [Nat] -> Exp Bool

type instance
  Eval (LTE xs ys) =
    Eval
      ( LiftM2
          (Fcf.&&)
          (And =<< ZipWith (Fcf.<=) xs ys)
          (LiftM2 TyEq (Rank xs) (Rank ys))
      )

-- | Check if a shape is a subset (<=) another shape after reranking.
--
-- >>> isSubset (VU.fromList [2,3,4]) (VU.fromList [2,3,4])
-- True
--
-- >>> isSubset (VU.fromList [1,2]) (VU.fromList [2,3,4])
-- True
--
-- >>> isSubset (VU.fromList [2,1]) (VU.fromList [1])
-- False
isSubset :: VU.Vector Int -> VU.Vector Int -> Bool
isSubset xs ys = lte (rerank (rank ys) xs) ys

-- | Check if a shape is a subset (<=) another shape after reranking.
--
-- >>> isSubsetL [2,3,4] [2,3,4]
-- True
--
-- >>> isSubsetL [1,2] [2,3,4]
-- True
--
-- >>> isSubsetL [2,1] [1]
-- False
isSubsetL :: [Int] -> [Int] -> Bool
isSubsetL xs ys = lteL (rerankL (rankL ys) xs) ys

-- | Check if a shape is a subset (<=) another shape after reranking.
--
-- >>> :k! Eval (IsSubset [2,3,4] [2,3,4])
-- ...
-- = True
--
-- >>> :k! Eval (IsSubset [1,2] [2,3,4])
-- ...
-- = True
--
-- >>> :k! Eval (IsSubset [2,1] '[1])
-- ...
-- = False
data IsSubset :: [Nat] -> [Nat] -> Exp Bool

type instance
  Eval (IsSubset xs ys) =
    Eval (LTE (Eval (Rerank (Eval (Rank ys)) xs)) ys)

-- | Compute dimensions for a shape other than the supplied dimensions.
--
-- >>> exceptDims (VU.fromList [1,2]) (VU.fromList [2,3,4])
-- [0]
exceptDims :: VU.Vector Int -> VU.Vector Int -> VU.Vector Int
exceptDims ds s = deleteDims ds (VU.enumFromTo 0 (rank s - 1))

-- | Compute dimensions for a shape other than the supplied dimensions.
--
-- >>> exceptDimsL [1,2] [2,3,4]
-- [0]
exceptDimsL :: [Int] -> [Int] -> [Int]
exceptDimsL ds s = deleteDimsL ds [0 .. (rankL s - 1)]

-- | Compute dimensions for a shape other than the supplied dimensions.
--
-- >>> :k! Eval (ExceptDims [1,2] [2,3,4])
-- ...
-- = '[0]
data ExceptDims :: [Nat] -> [Nat] -> Exp [Nat]

type instance
  Eval (ExceptDims ds s) =
    Eval (DeleteDims ds =<< EnumFromTo 0 (Eval ((Fcf.-) (Eval (Rank s)) 1)))

-- | Reorder the dimensions of shape according to a list of positions.
--
-- >>> reorder (VU.fromList [2,3,4]) (VU.fromList [2,0,1])
-- [4,2,3]
reorder :: VU.Vector Int -> VU.Vector Int -> VU.Vector Int
reorder s ds = VU.map (`getDim` s) ds

-- | Reorder the dimensions of shape according to a list of positions.
--
-- >>> reorderL [2,3,4] [2,0,1]
-- [4,2,3]
reorderL :: [Int] -> [Int] -> [Int]
reorderL s (d : ds) = getDimL d s : reorderL s ds
reorderL _ _ = []

-- | Reorder the dimensions of shape according to a list of positions.
--
-- >>> :k! Eval (Reorder [2,3,4] [2,0,1])
-- ...
-- = [4, 2, 3]
data Reorder :: [Nat] -> [Nat] -> Exp [Nat]

type instance
  Eval (Reorder ds xs) =
    If
      (Eval (ReorderOk ds xs))
      (Eval (Map (Flip GetDim ds) xs))
      (L.TypeError ('Text "Reorder dimension indices out of bounds"))

-- | Test if a Reorder is valid.
--
-- >>> :k! Eval (ReorderOk [2,3,4] [0,1])
-- ...
-- = False
data ReorderOk :: [Nat] -> [Nat] -> Exp Bool

type instance
  Eval (ReorderOk ds xs) =
    Eval (TyEq (Eval (Rank ds)) (Eval (Rank xs)))
      && Eval (And =<< Map (Flip IsFin (Eval (Rank ds))) xs)

-- | remove 1's from a list
--
-- >>> squeeze (VU.fromList [0,1,2,3])
-- [0,2,3]
squeeze :: VU.Vector Int -> VU.Vector Int
squeeze = VU.filter (/= 1)

-- | remove 1's from a list
--
-- >>> squeezeL [0,1,2,3]
-- [0,2,3]
squeezeL :: [Int] -> [Int]
squeezeL = filter (/= 1)

-- | Remove 1's from a list.
--
-- >>> :k! (Eval (Squeeze [0,1,2,3]))
-- ...
-- = [0, 2, 3]
data Squeeze :: [a] -> Exp [a]

type instance
  Eval (Squeeze xs) =
    Eval (Filter (Not <=< TyEq 1) xs)

-- | minimum of a list
--
-- >>> S.minimum (VU.fromList [2,3,4])
-- 2
minimum :: VU.Vector Int -> Int
minimum v
  | VU.null v = error "zero-ranked"
  | VU.length v == 1 = VU.head v
  | otherwise = VU.minimum v

-- | minimum of a list
--
-- >>> S.minimumL [2,3,4]
-- 2
minimumL :: [Int] -> Int
minimumL [] = error "zero-ranked"
minimumL [x] = x
minimumL (x : xs) = P.min x (minimumL xs)

-- | minimum of a list
--
-- >>> :k! Eval (Minimum '[])
-- ...
-- = (TypeError ...)
--
-- >>> :k! Eval (Minimum [2,3,4])
-- ...
-- = 2
data Minimum :: [a] -> Exp a

type instance Eval (Minimum '[]) = L.TypeError (L.Text "zero ranked")

type instance
  Eval (Minimum (x ': xs)) =
    Eval (Foldr Min x xs)

-- | Minimum of two type values.
--
-- >>> :k! Eval (Min 0 1)
-- ...
-- = 0
data Min :: a -> a -> Exp a

type instance Eval (Min a b) = If (a <? b) a b

-- | Maximum of two type values.
--
-- >>> :k! Eval (Max 0 1)
-- ...
-- = 1
data Max :: a -> a -> Exp a

type instance Eval (Max a b) = If (a >? b) a b

-- | Check if i is a valid Fin (aka in-bounds index of a dimension)
--
-- >>> isFin 0 2
-- True
-- >>> isFin 2 2
-- False
isFin :: Int -> Int -> Bool
isFin i d = 0 <= i && i + 1 <= d

-- | Check if i is a valid Fin (aka in-bounds index of a dimension)
--
-- >>> :k! Eval (IsFin 0 2)
-- ...
-- = True
-- >>> :k! Eval (IsFin 2 2)
-- ...
-- = False
data IsFin :: Nat -> Nat -> Exp Bool

type instance
  Eval (IsFin x d) =
    x <? d

-- | Check if i is a valid Fins (aka in-bounds index of a Shape)
--
-- >>> isFins (VU.fromList [0,1]) (VU.fromList [2,2])
-- True
-- >>> isFins (VU.fromList [0,1]) (VU.fromList [2,1])
-- False
isFins :: VU.Vector Int -> VU.Vector Int -> Bool
isFins xs ds = VU.length xs == VU.length ds && VU.and (VU.zipWith isFin xs ds)

-- | Check if i is a valid Fins (aka in-bounds index of a Shape)
--
-- >>> isFinsL [0,1] [2,2]
-- True
-- >>> isFinsL [0,1] [2,1]
-- False
isFinsL :: [Int] -> [Int] -> Bool
isFinsL xs ds = length xs == length ds && and (zipWith isFin xs ds)

-- | Check if i is a valid Fins (aka in-bounds index of a Shape)
--
-- >>> :k! Eval (IsFins [0,1] [2,2])
-- ...
-- = True
-- >>> :k! Eval (IsFins [0,1] [2,1])
-- ...
-- = False
data IsFins :: [Nat] -> [Nat] -> Exp Bool

type instance
  Eval (IsFins xs ds) =
    Eval (And (Eval (ZipWith IsFin xs ds)))
      && Eval (LiftM2 TyEq (Rank xs) (Rank ds))

-- | Is a value a valid dimension of a shape.
--
-- >>> isDim 2 (VU.fromList [2,3,4])
-- True
-- >>> isDim 0 (VU.fromList [])
-- True
isDim :: Int -> VU.Vector Int -> Bool
isDim d s = isFin d (rank s) || d == 0 && VU.null s

-- | Is a value a valid dimension of a shape.
--
-- >>> isDimL 2 [2,3,4]
-- True
-- >>> isDimL 0 []
-- True
isDimL :: Int -> [Int] -> Bool
isDimL d s = isFin d (rankL s) || d == 0 && null s

-- | Is a value a valid dimension of a shape.
--
-- >>> :k! Eval (IsDim 2 [2,3,4])
-- ...
-- = True
-- >>> :k! Eval (IsDim 0 '[])
-- ...
-- = True
data IsDim :: Nat -> [Nat] -> Exp Bool

type instance
  Eval (IsDim d s) =
    Eval (IsFin d =<< Rank s)
      || (0 == d && s == '[])

-- | Are values valid dimensions of a shape.
--
-- >>> isDims (VU.fromList [2,1]) (VU.fromList [2,3,4])
-- True
-- >>> isDims (VU.fromList [0]) (VU.fromList [])
-- True
isDims :: VU.Vector Int -> VU.Vector Int -> Bool
isDims ds s = VU.all (`isDim` s) ds

-- | Are values valid dimensions of a shape.
--
-- >>> isDimsL [2,1] [2,3,4]
-- True
-- >>> isDimsL [0] []
-- True
isDimsL :: [Int] -> [Int] -> Bool
isDimsL ds s = all (`isDimL` s) ds

-- | Are values valid dimensions of a shape.
--
-- >>> :k! Eval (IsDims [2,1] [2,3,4])
-- ...
-- = True
-- >>> :k! Eval (IsDims '[0] '[])
-- ...
-- = True
data IsDims :: [Nat] -> [Nat] -> Exp Bool

type instance
  Eval (IsDims ds s) =
    Eval (And =<< Map (Flip IsDim s) ds)

-- | Get the last position of a dimension of a shape.
--
-- >>> lastPos 2 (VU.fromList [2,3,4])
-- 3
-- >>> lastPos 0 (VU.fromList [])
-- 0
lastPos :: Int -> VU.Vector Int -> Int
lastPos d s =
  bool (getDim d s - 1) 0 (0 == d && VU.null s)

-- | Get the last position of a dimension of a shape.
--
-- >>> lastPosL 2 [2,3,4]
-- 3
-- >>> lastPosL 0 []
-- 0
lastPosL :: Int -> [Int] -> Int
lastPosL d s =
  bool (getDimL d s - 1) 0 (0 == d && null s)

-- | Get the last position of a dimension of a shape.
--
-- >>> :k! Eval (LastPos 2 [2,3,4])
-- ...
-- = 3
-- >>> :k! Eval (LastPos 0 '[])
-- ...
-- = 0
data LastPos :: Nat -> [Nat] -> Exp Nat

type instance
  Eval (LastPos d s) =
    If
      (0 == d && s == '[])
      0
      (Eval (GetDim d s) - 1)

-- | Get the minimum dimension as a singleton dimension.
--
-- >>> minDim (VU.fromList [2,3,4])
-- [2]
-- >>> minDim (VU.fromList [])
-- []
minDim :: VU.Vector Int -> VU.Vector Int
minDim v
  | VU.null v = VU.empty
  | otherwise = VU.singleton (minimum v)

-- | Get the minimum dimension as a singleton dimension.
--
-- >>> minDimL [2,3,4]
-- [2]
-- >>> minDimL []
-- []
minDimL :: [Int] -> [Int]
minDimL [] = []
minDimL s = [minimumL s]

-- | Get the minimum dimension as a singleton dimension.
--
-- >>> :k! Eval (MinDim [2,3,4])
-- ...
-- = '[2]
-- >>> :k! Eval (MinDim '[])
-- ...
-- = '[]
data MinDim :: [Nat] -> Exp [Nat]

type instance
  Eval (MinDim s) =
    If
      (s == '[])
      '[]
      '[Eval (Minimum s)]

-- | Left fold.
--
-- >>> :k! Eval (Foldl' (Fcf.+) 0 [1,2,3])
-- ...
-- = 6
data Foldl' :: (b -> a -> Exp b) -> b -> t a -> Exp b

type instance Eval (Foldl' f y '[]) = y

type instance Eval (Foldl' f y (x ': xs)) = Eval (Foldl' f (Eval (f y x)) xs)

-- | Get an element at a given index.
--
-- >>> :kind! Eval (GetIndex 2 [2,3,4])
-- ...
-- = Just 4
data GetIndex :: Nat -> [a] -> Exp (Maybe a)

type instance Eval (GetIndex d xs) = GetIndexImpl d xs

type family GetIndexImpl (n :: Nat) (xs :: [k]) where
  GetIndexImpl _ '[] = 'Nothing
  GetIndexImpl 0 (x ': _) = 'Just x
  GetIndexImpl n (_ ': xs) = GetIndexImpl (n - 1) xs

-- | Get the dimension of a shape at the supplied index. Error if out-of-bounds.
--
-- >>> getDim 1 (VU.fromList [2,3,4])
-- 3
-- >>> getDim 3 (VU.fromList [2,3,4])
-- *** Exception: getDim outside bounds
-- ...
-- >>> getDim 0 (VU.fromList [])
-- 1
getDim :: Int -> VU.Vector Int -> Int
getDim 0 v | VU.null v = 1
getDim i s = fromMaybe (error "getDim outside bounds") (maybeGetDim s i)

-- | Get the dimension of a shape at the supplied index. Error if out-of-bounds.
--
-- >>> getDimL 1 [2,3,4]
-- 3
-- >>> getDimL 3 [2,3,4]
-- *** Exception: getDim outside bounds
-- ...
-- >>> getDimL 0 []
-- 1
getDimL :: Int -> [Int] -> Int
getDimL 0 [] = 1
getDimL i s = fromMaybe (error "getDim outside bounds") (maybeGetDimL s i)

maybeGetDim :: VU.Vector Int -> Int -> Maybe Int
maybeGetDim v n
  | n < 0 = Nothing
  | n >= VU.length v = Nothing
  | otherwise = Just (VU.unsafeIndex v n)
{-# INLINEABLE maybeGetDim #-}

maybeGetDimL :: [a] -> Int -> Maybe a
maybeGetDimL xs n
  | n < 0 = Nothing
  | otherwise =
      foldr
        ( \x r k -> case k of
            0 -> Just x
            _ -> r (k - 1)
        )
        (const Nothing)
        xs
        n
{-# INLINEABLE maybeGetDimL #-}

-- | Get the dimension of a shape at the supplied index. Error if out-of-bounds or non-computable (usually unknown to the compiler).
--
-- >>> :k! Eval (GetDim 1 [2,3,4])
-- ...
-- = 3
-- >>> :k! Eval (GetDim 3 [2,3,4])
-- ...
-- = (TypeError ...)
-- >>> :k! Eval (GetDim 0 '[])
-- ...
-- = 1
data GetDim :: Nat -> [Nat] -> Exp Nat

type instance
  Eval (GetDim n xs) =
    If
      (Eval (And [Eval (TyEq n 0), Eval (TyEq xs ('[] :: [Nat]))]))
      1
      (Eval (FromMaybe (L.TypeError (L.Text "GetDim out of bounds or non-computable: " :<>: ShowType n :<>: L.Text " " :<>: ShowType xs)) (Eval (GetIndex n xs))))

-- | modify an index at a specific dimension. Errors if out of bounds.
--
-- >>> modifyDim 0 (+1) (VU.fromList [0,1,2])
-- [1,1,2]
-- >>> modifyDim 0 (+1) (VU.fromList [])
-- [2]
modifyDim :: Int -> (Int -> Int) -> VU.Vector Int -> VU.Vector Int
modifyDim 0 f v
  | VU.null v = VU.singleton (f 1)
modifyDim d f v =
  let x = getDim d v
      x' = f x
   in VU.take d v VU.++ VU.singleton x' VU.++ VU.drop (d + 1) v

-- | modify an index at a specific dimension. Errors if out of bounds.
--
-- >>> modifyDimL 0 (+1) [0,1,2]
-- [1,1,2]
-- >>> modifyDimL 0 (+1) []
-- [2]
modifyDimL :: Int -> (Int -> Int) -> [Int] -> [Int]
modifyDimL 0 f [] = [f 1]
modifyDimL d f xs =
  getDimL d xs
    & f
    & (: drop (d + 1) xs)
    & (take d xs <>)

-- | modify an index at a specific dimension. Errors if out of bounds.
--
-- >>> :k! Eval (ModifyDim 0 ((Fcf.+) 1) [0,1,2])
-- ...
-- = [1, 1, 2]
data ModifyDim :: Nat -> (Nat -> Exp Nat) -> [Nat] -> Exp [Nat]

type instance
  Eval (ModifyDim d f s) =
    Eval (LiftM2 (Fcf.++) (Take d s) (LiftM2 Cons (f =<< GetDim d s) (Drop (d + 1) s)))

-- | Increment the index at a dimension of a shape by 1. Scalars turn into singletons.
--
-- >>> incAt 1 (VU.fromList [2,3,4])
-- [2,4,4]
-- >>> incAt 0 (VU.fromList [])
-- [2]
incAt :: Int -> VU.Vector Int -> VU.Vector Int
incAt d ds = modifyDim d (+ 1) (asSingleton ds)

-- | Increment the index at a dimension of a shape by 1. Scalars turn into singletons.
--
-- >>> incAtL 1 [2,3,4]
-- [2,4,4]
-- >>> incAtL 0 []
-- [2]
incAtL :: Int -> [Int] -> [Int]
incAtL d ds = modifyDimL d (+ 1) (asSingletonL ds)

-- | Increment the index at a dimension of a shape by 1. Scalars turn into singletons.
--
-- >>> :k! Eval (IncAt 1 [2,3,4])
-- ...
-- = [2, 4, 4]
-- >>> :k! Eval (IncAt 0 '[])
-- ...
-- = '[2]
data IncAt :: Nat -> [Nat] -> Exp [Nat]

type instance
  Eval (IncAt d ds) =
    Eval (ModifyDim d ((Fcf.+) 1) (Eval (AsSingleton ds)))

-- | Decrement the index at a dimension os a shape by 1.
--
-- >>> decAt 1 (VU.fromList [2,3,4])
-- [2,2,4]
decAt :: Int -> VU.Vector Int -> VU.Vector Int
decAt d = modifyDim d (\x -> x - 1)

-- | Decrement the index at a dimension of a shape by 1.
--
-- >>> decAtL 1 [2,3,4]
-- [2,2,4]
decAtL :: Int -> [Int] -> [Int]
decAtL d = modifyDimL d (\x -> x - 1)

-- | Decrement the index at a dimension of a shape by 1.
--
-- >>> :k! Eval (DecAt 1 [2,3,4])
-- ...
-- = [2, 2, 4]
data DecAt :: Nat -> [Nat] -> Exp [Nat]

type instance
  Eval (DecAt d ds) =
    Eval (ModifyDim d (Flip (Fcf.-) 1) ds)

-- | replace an index at a specific dimension, or transform a scalar into being 1-dimensional.
--
-- >>> setDim 0 1 (VU.fromList [2,3,4])
-- [1,3,4]
-- >>> setDim 0 3 (VU.fromList [])
-- [3]
setDim :: Int -> Int -> VU.Vector Int -> VU.Vector Int
setDim d x = modifyDim d (const x)

-- | replace an index at a specific dimension, or transform a scalar into being 1-dimensional.
--
-- >>> setDimL 0 1 [2,3,4]
-- [1,3,4]
-- >>> setDimL 0 3 []
-- [3]
setDimL :: Int -> Int -> [Int] -> [Int]
setDimL d x = modifyDimL d (const x)

-- | replace an index at a specific dimension.
--
-- >>> :k! Eval (SetDim 0 1 [2,3,4])
-- ...
-- = [1, 3, 4]
data SetDim :: Nat -> Nat -> [Nat] -> Exp [Nat]

type instance
  Eval (SetDim d x ds) =
    Eval (ModifyDim d (ConstFn x) ds)

data SetDimUncurried :: (Nat, Nat) -> [Nat] -> Exp [Nat]

type instance
  Eval (SetDimUncurried xs ds) =
    Eval (SetDim (Eval (Fst xs)) (Eval (Snd xs)) ds)

-- | Halve a dimension size (integer division by 2).
--
-- >>> halfDim 5
-- 2
halfDim :: Int -> Int
halfDim n = n `P.div` 2

-- | Halve a type-level natural.
--
-- >>> :k! Eval (Half 5)
-- Eval (Half 5) :: ghc-internal:GHC.Internal.Bignum.Natural.Natural
-- = 2
data Half :: Nat -> Exp Nat

type instance
  Eval (Half n) =
    Div n 2

-- | Halve a dimension of a shape.
--
-- >>> :k! Eval (HalveDim 0 [5, 7])
-- Eval (HalveDim 0 [5, 7]) :: [ghc-internal:GHC.Internal.Bignum.Natural.Natural]
-- = [2, 7]
data HalveDim :: Nat -> [Nat] -> Exp [Nat]

type instance
  Eval (HalveDim d s) =
    Eval (ModifyDim d Half s)

-- | Take along a dimension.
--
-- >>> takeDim 0 1 (VU.fromList [2,3,4])
-- [1,3,4]
takeDim :: Int -> Int -> VU.Vector Int -> VU.Vector Int
takeDim d t = modifyDim d (P.min t)

-- | Take along a dimension.
--
-- >>> takeDimL 0 1 [2,3,4]
-- [1,3,4]
takeDimL :: Int -> Int -> [Int] -> [Int]
takeDimL d t = modifyDimL d (P.min t)

-- | Take along a dimension.
--
-- >>> :k! Eval (TakeDim 0 1 [2,3,4])
-- ...
-- = [1, 3, 4]
data TakeDim :: Nat -> Nat -> [Nat] -> Exp [Nat]

type instance
  Eval (TakeDim d t s) =
    Eval
      (ModifyDim d (Min t) s)

-- | Drop along a dimension.
--
-- >>> dropDim 2 1 (VU.fromList [2,3,4])
-- [2,3,3]
dropDim :: Int -> Int -> VU.Vector Int -> VU.Vector Int
dropDim d t = modifyDim d (P.max 0 . (\x -> x - t))

-- | Drop along a dimension.
--
-- >>> dropDimL 2 1 [2,3,4]
-- [2,3,3]
dropDimL :: Int -> Int -> [Int] -> [Int]
dropDimL d t = modifyDimL d (P.max 0 . (\x -> x - t))

-- | Drop along a dimension.
--
-- >>> :k! Eval (DropDim 2 1 [2,3,4])
-- ...
-- = [2, 3, 3]
data DropDim :: Nat -> Nat -> [Nat] -> Exp [Nat]

type instance
  Eval (DropDim d t s) =
    Eval
      ( ModifyDim
          d
          (Max 0 <=< Flip (Fcf.-) t)
          s
      )

-- | delete the i'th dimension. No effect on a scalar.
--
-- >>> deleteDim 1 (VU.fromList [2, 3, 4])
-- [2,4]
-- >>> deleteDim 2 (VU.fromList [])
-- []
deleteDim :: Int -> VU.Vector Int -> VU.Vector Int
deleteDim i s = VU.take i s VU.++ VU.drop (i + 1) s

-- | delete the i'th dimension. No effect on a scalar.
--
-- >>> deleteDimL 1 [2, 3, 4]
-- [2,4]
-- >>> deleteDimL 2 []
-- []
deleteDimL :: Int -> [Int] -> [Int]
deleteDimL i s = take i s ++ drop (i + 1) s

-- | delete the i'th dimension
--
-- >>> :k! Eval (DeleteDim 1 [2, 3, 4])
-- ...
-- = [2, 4]
-- >>> :k! Eval (DeleteDim 1 '[])
-- ...
-- = '[]
data DeleteDim :: Nat -> [Nat] -> Exp [Nat]

type instance
  Eval (DeleteDim i ds) =
    Eval (LiftM2 (Fcf.++) (Take i ds) (Drop (i + 1) ds))

-- | Insert a new dimension at a position (or at the end if > rank).
--
-- >>> insertDim 1 3 (VU.fromList [2,4])
-- [2,3,4]
-- >>> insertDim 0 4 (VU.fromList [])
-- [4]
insertDim :: Int -> Int -> VU.Vector Int -> VU.Vector Int
insertDim d i s = VU.take d s VU.++ VU.cons i (VU.drop d s)

-- | Insert a new dimension at a position (or at the end if > rank).
--
-- >>> insertDimL 1 3 [2,4]
-- [2,3,4]
-- >>> insertDimL 0 4 []
-- [4]
insertDimL :: Int -> Int -> [Int] -> [Int]
insertDimL d i s = take d s ++ (i : drop d s)

-- | Insert a new dimension at a position (or at the end if > rank).
--
-- >>> :k! Eval (InsertDim 1 3 [2,4])
-- ...
-- = [2, 3, 4]
-- >>> :k! Eval (InsertDim 0 4 '[])
-- ...
-- = '[4]
data InsertDim :: Nat -> Nat -> [Nat] -> Exp [Nat]

type instance
  Eval (InsertDim d i ds) =
    Eval (LiftM2 (Fcf.++) (Take d ds) (Cons i =<< Drop d ds))

data InsertDimUncurried :: (Nat, Nat) -> [Nat] -> Exp [Nat]

type instance
  Eval (InsertDimUncurried xs ds) =
    Eval (InsertDim (Eval (Fst xs)) (Eval (Snd xs)) ds)

-- | Is a slice ok constraint.
--
-- >>> :k! Eval (InsertOk 2 [2,3,4] [2,3])
-- ...
-- = True
-- >>> :k! Eval (InsertOk 0 '[] '[])
-- ...
-- = True
data InsertOk :: Nat -> [Nat] -> [Nat] -> Exp Bool

type instance
  Eval (InsertOk d s si) =
    Eval
      ( And
          [ Eval (IsDim d s),
            Eval (TyEq si (Eval (DeleteDim d s)))
          ]
      )

-- | Is a slice ok?
--
-- >>> :k! Eval (SliceOk 1 1 2 [2,3,4])
-- ...
-- = True
data SliceOk :: Nat -> Nat -> Nat -> [Nat] -> Exp Bool

type instance
  Eval (SliceOk d off l s) =
    Eval
      ( And
          [ Eval (IsFin off =<< GetDim d s),
            Eval ((Fcf.<) l =<< GetDim d s),
            Eval ((Fcf.<) (off + l) (Eval (GetDim d s) + 1)),
            Eval (IsDim d s)
          ]
      )

-- | Combine elements of two lists pairwise.
data ZipWith3 :: (a -> b -> c -> Exp d) -> [a] -> [b] -> [c] -> Exp [d]

type instance Eval (ZipWith3 _f '[] _bs _cs) = '[]

type instance Eval (ZipWith3 _f _as '[] _cs) = '[]

type instance Eval (ZipWith3 _f _as _bs '[]) = '[]

type instance
  Eval (ZipWith3 f (a ': as) (b ': bs) (c ': cs)) =
    Eval (f a b c) ': Eval (ZipWith3 f as bs cs)

data SliceOk_ :: [Nat] -> Nat -> Nat -> Nat -> Exp Bool

type instance Eval (SliceOk_ s d off l) = Eval (SliceOk d off l s)

-- | Are slices ok?
--
-- >>> :k! Eval (SlicesOk '[1] '[1] '[2] [2,3,4])
-- ...
-- = True
data SlicesOk :: [Nat] -> [Nat] -> [Nat] -> [Nat] -> Exp Bool

type instance
  Eval (SlicesOk ds offs ls s) =
    Eval (And =<< ZipWith3 (SliceOk_ s) ds offs ls)

-- | concatenate two arrays at dimension i
--
-- Bespoke logic for scalars.
--
-- >>> concatenate 1 (VU.fromList [2,3,4]) (VU.fromList [2,3,4])
-- [2,6,4]
-- >>> concatenate 0 (VU.fromList [3]) (VU.fromList [])
-- [4]
-- >>> concatenate 0 (VU.fromList []) (VU.fromList [3])
-- [4]
-- >>> concatenate 0 (VU.fromList []) (VU.fromList [])
-- [2]
concatenate :: Int -> VU.Vector Int -> VU.Vector Int -> VU.Vector Int
concatenate _ v0 v1
  | VU.null v0 && VU.null v1 = VU.singleton 2
  | VU.null v0 && VU.length v1 == 1 = VU.singleton (VU.head v1 + 1)
  | VU.length v0 == 1 && VU.null v1 = VU.singleton (VU.head v0 + 1)
concatenate i s0 s1 = VU.take i s0 VU.++ VU.cons (getDim i s0 + getDim i s1) (VU.drop (i + 1) s0)

-- | concatenate two arrays at dimension i
--
-- Bespoke logic for scalars.
--
-- >>> concatenateL 1 [2,3,4] [2,3,4]
-- [2,6,4]
-- >>> concatenateL 0 [3] []
-- [4]
-- >>> concatenateL 0 [] [3]
-- [4]
-- >>> concatenateL 0 [] []
-- [2]
concatenateL :: Int -> [Int] -> [Int] -> [Int]
concatenateL _ [] [] = [2]
concatenateL _ [] [x] = [x + 1]
concatenateL _ [x] [] = [x + 1]
concatenateL i s0 s1 = take i s0 ++ (getDimL i s0 + getDimL i s1 : drop (i + 1) s0)

-- | concatenate two arrays at dimension i
--
-- Bespoke logic for scalars.
--
-- >>> :k! Eval (Concatenate 1 [2,3,4] [2,3,4])
-- ...
-- = [2, 6, 4]
-- >>> :k! Eval (Concatenate 0 '[3] '[])
-- ...
-- = '[4]
-- >>> :k! Eval (Concatenate 0 '[] '[3])
-- ...
-- = '[4]
-- >>> :k! Eval (Concatenate 0 '[] '[])
-- ...
-- = '[2]
data Concatenate :: Nat -> [Nat] -> [Nat] -> Exp [Nat]

type instance
  Eval (Concatenate i s0 s1) =
    If
      (Eval (ConcatenateOk i s0 s1))
      (Eval (Eval (Take i s0) ++ (Eval (GetDim i s0) + Eval (GetDim i s1) : Eval (Drop (i + 1) s0))))
      (L.TypeError (L.Text "Concatenate Mis-matched shapes."))

-- | Concatenate is Ok if ranks are the same and the non-indexed portion of the shapes are the same.
data ConcatenateOk :: Nat -> [Nat] -> [Nat] -> Exp Bool

type instance
  Eval (ConcatenateOk i s0 s1) =
    Eval (IsDim i s0)
      && Eval (IsDim i s1)
      && Eval (LiftM2 TyEq (DeleteDim i s0) (DeleteDim i s1))
      && Eval (LiftM2 TyEq (Rank =<< AsSingleton s0) (Rank =<< AsSingleton s1))

-- * multiple dimension manipulations

-- | Get dimensions of a shape.
--
-- >>> getDims (VU.fromList [2,0]) (VU.fromList [2,3,4])
-- [4,2]
-- >>> getDims (VU.fromList [2]) (VU.fromList [])
-- []
getDims :: VU.Vector Int -> VU.Vector Int -> VU.Vector Int
getDims _ v | VU.null v = VU.empty
getDims i s = VU.map (`getDim` s) i

-- | Get dimensions of a shape.
--
-- >>> getDimsL [2,0] [2,3,4]
-- [4,2]
-- >>> getDimsL [2] []
-- []
getDimsL :: [Int] -> [Int] -> [Int]
getDimsL _ [] = []
getDimsL i s = (`getDimL` s) <$> i

-- | Get dimensions of a shape.
--
-- >>> :k! Eval (GetDims [2,0] [2,3,4])
-- ...
-- = [4, 2]
-- >>> :k! Eval (GetDims '[2] '[])
-- ...
-- = '[(TypeError ...)]
data GetDims :: [Nat] -> [Nat] -> Exp [Nat]

type instance
  Eval (GetDims xs ds) =
    Eval (Map (Flip GetDim ds) xs)

-- | Get the index of the last position in the selected dimensions of a shape. Errors on a 0-dimension.
--
-- >>> getLastPositions (VU.fromList [2,0]) (VU.fromList [2,3,4])
-- [3,1]
-- >>> getLastPositions (VU.fromList [0]) (VU.fromList [0])
-- [-1]
getLastPositions :: VU.Vector Int -> VU.Vector Int -> VU.Vector Int
getLastPositions ds s =
  VU.map (\x -> x - 1) (getDims ds s)

-- | Get the index of the last position in the selected dimensions of a shape. Errors on a 0-dimension.
--
-- >>> getLastPositionsL [2,0] [2,3,4]
-- [3,1]
-- >>> getLastPositionsL [0] [0]
-- [-1]
getLastPositionsL :: [Int] -> [Int] -> [Int]
getLastPositionsL ds s =
  fmap (\x -> x - 1) (getDimsL ds s)

-- | Get the index of the last position in the selected dimensions of a shape. Errors on a 0-dimension.
--
-- >>> :k! Eval (GetLastPositions [2,0] [2,3,4])
-- ...
-- = [3, 1]
data GetLastPositions :: [Nat] -> [Nat] -> Exp [Nat]

type instance
  Eval (GetLastPositions ds s) =
    Eval (Map (Flip (Fcf.-) 1) (Eval (GetDims ds s)))

-- | modify dimensions of a shape with (separate) functions.
--
-- >>> modifyDims (VU.fromList [0,1]) [(+1), (+5)] (VU.fromList [2,3,4])
-- [3,8,4]
modifyDims :: VU.Vector Int -> [Int -> Int] -> VU.Vector Int -> VU.Vector Int
modifyDims ds fs ns = foldl' (\ns' (d, f) -> modifyDim d f ns') ns (VU.toList ds `zip` fs)

-- | modify dimensions of a shape with (separate) functions.
--
-- >>> modifyDimsL [0,1] [(+1), (+5)] [2,3,4]
-- [3,8,4]
modifyDimsL :: [Int] -> [Int -> Int] -> [Int] -> [Int]
modifyDimsL ds fs ns = foldl' (\ns' (d, f) -> modifyDimL d f ns') ns (zip ds fs)

-- | Convert a list of positions that reference deletions according to a final shape to 1 that references deletions relative to an initial shape.
--
-- To delete the positions [1,2,5] from a list, for example, you need to delete position 1, (arriving at a 4 element list), then position 1, arriving at a 3 element list, and finally position 3.
--
-- >>> preDeletePositions (VU.fromList [1,2,5])
-- [1,1,3]
--
-- >>> preDeletePositions (VU.fromList [1,2,0])
-- [1,1,0]
preDeletePositions :: VU.Vector Int -> VU.Vector Int
preDeletePositions as = VU.reverse (go as VU.empty)
  where
    go v r
      | VU.null v = r
      | otherwise = let x = VU.head v; xs = VU.tail v in go (VU.map (decPast x) xs) (VU.cons x r)
    decPast x y = bool (y - 1) y (y < x)

-- | Convert a list of positions that reference deletions according to a final shape to 1 that references deletions relative to an initial shape.
--
-- To delete the positions [1,2,5] from a list, for example, you need to delete position 1, (arriving at a 4 element list), then position 1, arriving at a 3 element list, and finally position 3.
--
-- >>> preDeletePositionsL [1,2,5]
-- [1,1,3]
--
-- >>> preDeletePositionsL [1,2,0]
-- [1,1,0]
preDeletePositionsL :: [Int] -> [Int]
preDeletePositionsL as = reverse (go as [])
  where
    go [] r = r
    go (x : xs) r = go (decPast x <$> xs) (x : r)
    decPast x y = bool (y - 1) y (y < x)

-- | Convert a list of positions that reference deletions according to a final shape to 1 that references deletions relative to an initial shape.
--
-- To delete the positions [1,2,5] from a list, for example, you need to delete position 1, (arriving at a 4 element list), then position 1, arriving at a 3 element list, and finally position 3.
--
-- >>> :k! Eval (PreDeletePositions [1,2,5])
-- ...
-- = [1, 1, 3]
--
-- >>> :k! Eval (PreDeletePositions [1,2,0])
-- ...
-- = [1, 1, 0]
data PreDeletePositions :: [Nat] -> Exp [Nat]

type instance
  Eval (PreDeletePositions xs) =
    Eval (Reverse (Eval (PreDeletePositionsGo xs '[])))

data PreDeletePositionsGo :: [Nat] -> [Nat] -> Exp [Nat]

type instance Eval (PreDeletePositionsGo '[] rs) = rs

type instance
  Eval (PreDeletePositionsGo (x : xs) r) =
    Eval (PreDeletePositionsGo (Eval (Map (DecPast x) xs)) (x : r))

data DecPast :: Nat -> Nat -> Exp Nat

type instance
  Eval (DecPast x d) =
    If (x + 1 <=? d) (d - 1) d

-- | Convert a list of position that reference insertions according to a final shape to 1 that references list insertions relative to an initial shape.
--
-- To insert into positions [1,2,0] from a list, starting from a 2 element list, for example, you need to insert at position 0, (arriving at a 3 element list), then position 1, arriving at a 4 element list, and finally position 0.
--
-- > preInsertPositions == reverse . preDeletePositions . reverse
-- >>> preInsertPositions (VU.fromList [1,2,5])
-- [1,2,5]
--
-- >>> preInsertPositions (VU.fromList [1,2,0])
-- [0,1,0]
preInsertPositions :: VU.Vector Int -> VU.Vector Int
preInsertPositions = VU.reverse . preDeletePositions . VU.reverse

-- | Convert a list of position that reference insertions according to a final shape to 1 that references list insertions relative to an initial shape.
--
-- To insert into positions [1,2,0] from a list, starting from a 2 element list, for example, you need to insert at position 0, (arriving at a 3 element list), then position 1, arriving at a 4 element list, and finally position 0.
--
-- > preInsertPositionsL == reverse . preDeletePositionsL . reverse
-- >>> preInsertPositionsL [1,2,5]
-- [1,2,5]
--
-- >>> preInsertPositionsL [1,2,0]
-- [0,1,0]
preInsertPositionsL :: [Int] -> [Int]
preInsertPositionsL = reverse . preDeletePositionsL . reverse

-- | Convert a list of position that reference insertions according to a final shape to 1 that references list insertions relative to an initial shape.
--
-- To insert into positions [1,2,0] from a list, starting from a 2 element list, for example, you need to insert at position 0, (arriving at a 3 element list), then position 1, arriving at a 4 element list, and finally position 0.
--
-- > preInsertPositions == reverse . preDeletePositions . reverse
-- >>> :k! Eval (PreInsertPositions [1,2,5])
-- ...
-- = [1, 2, 5]
--
-- >>> :k! Eval (PreInsertPositions [1,2,0])
-- ...
-- = [0, 1, 0]
data PreInsertPositions :: [Nat] -> Exp [Nat]

type instance
  Eval (PreInsertPositions xs) =
    Eval (Reverse =<< (PreDeletePositions =<< Reverse xs))

-- | drop dimensions of a shape according to a list of positions (where position refers to the initial shape)
--
-- >>> deleteDims (VU.fromList [1,0]) (VU.fromList [2, 3, 4])
-- [4]
deleteDims :: VU.Vector Int -> VU.Vector Int -> VU.Vector Int
deleteDims i s = VU.foldl' (flip deleteDim) s (preDeletePositions i)

-- | drop dimensions of a shape according to a list of positions (where position refers to the initial shape)
--
-- >>> deleteDimsL [1,0] [2, 3, 4]
-- [4]
deleteDimsL :: [Int] -> [Int] -> [Int]
deleteDimsL i s = foldl' (flip deleteDimL) s (preDeletePositionsL i)

-- | drop dimensions of a shape according to a list of positions (where position refers to the initial shape)
--
-- >>> :k! Eval (DeleteDims [1,0] [2, 3, 4])
-- ...
-- = '[4]
data DeleteDims :: [Nat] -> [Nat] -> Exp [Nat]

type instance
  Eval (DeleteDims xs ds) =
    Eval (Foldl' (Flip DeleteDim) ds =<< PreDeletePositions xs)

-- | Insert a list of dimensions according to dimensions and positions.  Note that the list of positions references the final shape and not the initial shape.
--
-- >>> insertDims (VU.fromList [0]) (VU.fromList [5]) (VU.fromList [])
-- [5]
-- >>> insertDims (VU.fromList [1,0]) (VU.fromList [3,2]) (VU.fromList [4])
-- [2,3,4]
insertDims :: VU.Vector Int -> VU.Vector Int -> VU.Vector Int -> VU.Vector Int
insertDims ds xs s = VU.foldl' (flip (uncurry insertDim)) s ps
  where
    ps = VU.zip (preInsertPositions ds) xs

-- | Insert a list of dimensions according to dimensions and positions.  Note that the list of positions references the final shape and not the initial shape.
--
-- >>> insertDimsL [0] [5] []
-- [5]
-- >>> insertDimsL [1,0] [3,2] [4]
-- [2,3,4]
insertDimsL :: [Int] -> [Int] -> [Int] -> [Int]
insertDimsL ds xs s = foldl' (flip (uncurry insertDimL)) s ps
  where
    ps = zip (preInsertPositionsL ds) xs

-- | insert a list of dimensions according to dimension,position tuple lists.  Note that the list of positions references the final shape and not the initial shape.
--
-- >>> :k! Eval (InsertDims '[0] '[5] '[])
-- ...
-- = '[5]
-- >>> :k! Eval (InsertDims [1,0] [3,2] '[4])
-- ...
-- = [2, 3, 4]
data InsertDims :: [Nat] -> [Nat] -> [Nat] -> Exp [Nat]

type instance
  Eval (InsertDims ds xs s) =
    Eval (Foldl' (Flip InsertDimUncurried) s =<< Flip Zip xs =<< PreInsertPositions ds)

-- | Set dimensions of a shape.
--
-- >>> setDims (VU.fromList [0,1]) (VU.fromList [1,5]) (VU.fromList [2,3,4])
-- [1,5,4]
--
-- >>> setDims (VU.fromList [0]) (VU.fromList [3]) (VU.fromList [])
-- [3]
setDims :: VU.Vector Int -> VU.Vector Int -> VU.Vector Int -> VU.Vector Int
setDims ds xs ns = VU.foldl' (\ns' (d, x) -> setDim d x ns') ns (VU.zip ds xs)

-- | Set dimensions of a shape.
--
-- >>> setDimsL [0,1] [1,5] [2,3,4]
-- [1,5,4]
--
-- >>> setDimsL [0] [3] []
-- [3]
setDimsL :: [Int] -> [Int] -> [Int] -> [Int]
setDimsL ds xs ns = foldl' (\ns' (d, x) -> setDimL d x ns') ns (zip ds xs)

-- | Set dimensions of a shape.
--
-- >>> :k! Eval (SetDims [0,1] [1,5] [2,3,4])
-- ...
-- = [1, 5, 4]
--
-- >>> :k! Eval (SetDims '[0] '[3] '[])
-- ...
-- = '[3]
data SetDims :: [Nat] -> [Nat] -> [Nat] -> Exp [Nat]

type instance
  Eval (SetDims ds xs ns) =
    Eval (Foldl' (Flip SetDimUncurried) ns =<< Zip ds xs)

-- | Drop a number of elements of a shape along the supplied dimensions.
--
-- >>> dropDims (VU.fromList [0,2]) (VU.fromList [1,3]) (VU.fromList [2,3,4])
-- [1,3,1]
dropDims :: VU.Vector Int -> VU.Vector Int -> VU.Vector Int -> VU.Vector Int
dropDims ds xs s = setDims ds xs' s
  where
    xs' = VU.zipWith (-) (getDims ds s) xs

-- | Drop a number of elements of a shape along the supplied dimensions.
--
-- >>> dropDimsL [0,2] [1,3] [2,3,4]
-- [1,3,1]
dropDimsL :: [Int] -> [Int] -> [Int] -> [Int]
dropDimsL ds xs s = setDimsL ds xs' s
  where
    xs' = zipWith (-) (getDimsL ds s) xs

-- | Drop a number of elements of a shape along the supplied dimensions.
--
-- >>> :k! Eval (DropDims [0,2] [1,3] [2,3,4])
-- ...
-- = [1, 3, 1]
data DropDims :: [Nat] -> [Nat] -> [Nat] -> Exp [Nat]

type instance
  Eval (DropDims ds xs s) =
    Eval (SetDims ds (Eval (ZipWith (Fcf.-) (Eval (GetDims ds s)) xs)) s)

-- | Concatenate and replace dimensions, creating a new dimension at the supplied postion.
--
-- >>> concatDims (VU.fromList [0,1]) 1 (VU.fromList [2,3,4])
-- [4,6]
concatDims :: VU.Vector Int -> Int -> VU.Vector Int -> VU.Vector Int
concatDims ds n s = insertDim n (size $ getDims ds s) (deleteDims ds s)

-- | Concatenate and replace dimensions, creating a new dimension at the supplied postion.
--
-- >>> concatDimsL [0,1] 1 [2,3,4]
-- [4,6]
concatDimsL :: [Int] -> Int -> [Int] -> [Int]
concatDimsL ds n s = insertDimL n (sizeL $ getDimsL ds s) (deleteDimsL ds s)

-- | Drop a number of elements of a shape along the supplied dimensions.
--
-- >>> :k! Eval (ConcatDims [0,1] 1 [2,3,4])
-- ...
-- = [4, 6]
data ConcatDims :: [Nat] -> Nat -> [Nat] -> Exp [Nat]

type instance
  Eval (ConcatDims ds n s) =
    Eval (InsertDim n (Eval (Size (Eval (GetDims ds s)))) (Eval (DeleteDims ds s)))

-- | Unconcatenate and reinsert dimensions for an index.
--
-- >>> unconcatDimsIndex (VU.fromList [0,1]) 1 (VU.fromList [4,6]) (VU.fromList [2,3])
-- [0,3,2]
unconcatDimsIndex :: VU.Vector Int -> Int -> VU.Vector Int -> VU.Vector Int -> VU.Vector Int
unconcatDimsIndex ds n s i = insertDims ds (shapen (getDims ds s) (getDim n i)) (deleteDim n i)

-- | Unconcatenate and reinsert dimensions for an index.
--
-- >>> unconcatDimsIndexL [0,1] 1 [4,6] [2,3]
-- [0,3,2]
unconcatDimsIndexL :: [Int] -> Int -> [Int] -> [Int] -> [Int]
unconcatDimsIndexL ds n s i = insertDimsL ds (shapenL (getDimsL ds s) (getDimL n i)) (deleteDimL n i)

-- | reverse an index along specific dimensions.
--
-- >>> reverseIndex (VU.fromList [0]) (VU.fromList [2,3,4]) (VU.fromList [0,1,2])
-- [1,1,2]
reverseIndex :: VU.Vector Int -> VU.Vector Int -> VU.Vector Int -> VU.Vector Int
reverseIndex ds ns xs = VU.map (\(i, x, n) -> bool x (n - 1 - x) (VU.elem i ds)) (VU.zip3 (VU.enumFromTo 0 (VU.length xs - 1)) xs ns)

-- | reverse an index along specific dimensions.
--
-- >>> reverseIndexL [0] [2,3,4] [0,1,2]
-- [1,1,2]
reverseIndexL :: [Int] -> [Int] -> [Int] -> [Int]
reverseIndexL ds ns xs = fmap (\(i, x, n) -> bool x (n - 1 - x) (i `elem` ds)) (zip3 [0 ..] xs ns)

-- | rotate a list
--
-- >>> rotate 1 (VU.fromList [0..3])
-- [1,2,3,0]
-- >>> rotate (-1) (VU.fromList [0..3])
-- [3,0,1,2]
rotate :: Int -> VU.Vector Int -> VU.Vector Int
rotate r xs = VU.drop r' xs VU.++ VU.take r' xs
  where
    r' = r `mod` VU.length xs

-- | rotate a list
--
-- >>> rotateL 1 [0..3]
-- [1,2,3,0]
-- >>> rotateL (-1) [0..3]
-- [3,0,1,2]
rotateL :: Int -> [a] -> [a]
rotateL r xs = drop r' xs <> take r' xs
  where
    r' = r `mod` List.length xs

-- | rotate an index along a specific dimension.
--
-- >>> rotateIndex 0 1 (VU.fromList [2,3,4]) (VU.fromList [0,1,2])
-- [1,1,2]
rotateIndex :: Int -> Int -> VU.Vector Int -> VU.Vector Int -> VU.Vector Int
rotateIndex d r s = modifyDim d (\x -> (x + r) `mod` getDim d s)

-- | rotate an index along a specific dimension.
--
-- >>> rotateIndexL 0 1 [2,3,4] [0,1,2]
-- [1,1,2]
rotateIndexL :: Int -> Int -> [Int] -> [Int] -> [Int]
rotateIndexL d r s = modifyDimL d (\x -> (x + r) `mod` getDimL d s)

-- | rotate an index along specific dimensions.
--
-- >>> rotatesIndex (VU.fromList [0]) (VU.fromList [1]) (VU.fromList [2,3,4]) (VU.fromList [0,1,2])
-- [1,1,2]
rotatesIndex :: VU.Vector Int -> VU.Vector Int -> VU.Vector Int -> VU.Vector Int -> VU.Vector Int
rotatesIndex ds rs s xs = VU.foldr (\(d, r) acc -> rotateIndex d r s acc) xs (VU.zip ds rs)

-- | rotate an index along specific dimensions.
--
-- >>> rotatesIndexL [0] [1] [2,3,4] [0,1,2]
-- [1,1,2]
rotatesIndexL :: [Int] -> [Int] -> [Int] -> [Int] -> [Int]
rotatesIndexL ds rs s xs = foldr (\(d, r) acc -> rotateIndexL d r s acc) xs (zip ds rs)

-- | Test whether an index is a diagonal one.
--
-- >>> isDiag (VU.fromList [2,2,2])
-- True
-- >>> isDiag (VU.fromList [1,2])
-- False
isDiag :: VU.Vector Int -> Bool
isDiag v
  | VU.length v <= 1 = True
  | VU.length v == 2 = VU.unsafeIndex v 0 == VU.unsafeIndex v 1
  | otherwise = VU.unsafeIndex v 0 == VU.unsafeIndex v 1 && isDiag (VU.tail v)

-- | Test whether an index is a diagonal one.
--
-- >>> isDiagL [2,2,2]
-- True
-- >>> isDiagL [1,2]
-- False
isDiagL :: (Eq a) => [a] -> Bool
isDiagL [] = True
isDiagL [_] = True
isDiagL [x, y] = x == y
isDiagL (x : y : xs) = x == y && isDiagL (y : xs)

-- | Expanded shape of a windowed array
--
-- >>> expandWindows (VU.fromList [2,2]) (VU.fromList [4,3,2])
-- [3,2,2,2,2]
expandWindows :: VU.Vector Int -> VU.Vector Int -> VU.Vector Int
expandWindows ws ds = VU.zipWith (\s' x' -> s' - x' + 1) ds ws VU.++ ws VU.++ VU.drop (rank ws) ds

-- | Expanded shape of a windowed array
--
-- >>> expandWindowsL [2,2] [4,3,2]
-- [3,2,2,2,2]
expandWindowsL :: [Int] -> [Int] -> [Int]
expandWindowsL ws ds = List.zipWith (\s' x' -> s' - x' + 1) ds ws <> ws <> List.drop (rankL ws) ds

-- | Expanded shape of a windowed array
--
-- >>> :k! Eval (ExpandWindows [2,2] [4,3,2])
-- ...
-- = [3, 2, 2, 2, 2]
data ExpandWindows :: [Nat] -> [Nat] -> Exp [Nat]

type instance
  Eval (ExpandWindows ws ds) =
    Eval (Eval (ZipWith (Fcf.-) (Eval (Map ((Fcf.+) 1) ds)) ws) ++ Eval (ws ++ Eval (Drop (Eval (Rank ws)) ds)))

-- | Index into windows of an expanded windowed array, given a rank of the windows.
--
-- >>> indexWindows 2 (VU.fromList [0,1,2,1,1])
-- [2,2,1]
indexWindows :: Int -> VU.Vector Int -> VU.Vector Int
indexWindows r ds = VU.zipWith (+) (VU.take r ds) (VU.take r (VU.drop r ds)) VU.++ VU.drop (r + r) ds

-- | Index into windows of an expanded windowed array, given a rank of the windows.
--
-- >>> indexWindowsL 2 [0,1,2,1,1]
-- [2,2,1]
indexWindowsL :: Int -> [Int] -> [Int]
indexWindowsL r ds = List.zipWith (+) (List.take r ds) (List.take r (List.drop r ds)) <> List.drop (r + r) ds

-- | Dimensions of a windowed array.
--
-- >>> dimWindows (VU.fromList [2,2]) (VU.fromList [2,3,4])
-- [0,1,2]
dimWindows :: VU.Vector Int -> VU.Vector Int -> VU.Vector Int
dimWindows ws s = range (rank s) VU.++ VU.enumFromTo (rank s * 2) (rank ws - 1)

-- | Dimensions of a windowed array.
--
-- >>> dimWindowsL [2,2] [2,3,4]
-- [0,1,2]
dimWindowsL :: [Int] -> [Int] -> [Int]
dimWindowsL ws s = rangeL (rankL s) <> [rankL s * 2 .. (rankL ws - 1)]

-- | Dimensions of a windowed array.
--
-- >>> :k! Eval (DimWindows [2,2] [4,3,2])
-- ...
-- = [0, 1, 2]
data DimWindows :: [Nat] -> [Nat] -> Exp [Nat]

type instance
  Eval (DimWindows ws s) =
    Eval (Eval (Range =<< Rank s) ++ Eval (EnumFromTo (Eval ((Fcf.*) 2 (Eval (Rank s)))) (Eval (Rank ws) - 1)))
