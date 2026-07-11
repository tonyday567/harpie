{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

-- | Arrays with shape information and computations at a value-level.
--
-- This module is a unboxed-vector facade over 'Harpie.Array.Generic'.
module Harpie.Array.Unboxed
  ( -- * Re-exports from the generic core
    module Harpie.Array.Generic,

    -- * Unboxed facade
    Array,
    array,
    (><),
    validate,
    safeArray,
    unsafeModifyVector,

    -- * Conversion
    FromVector (..),
    FromArray (..),
  )
where

import Data.Bool
import Data.Vector.Generic qualified as VG
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as VU
import Harpie.Array.Generic hiding
  ( Array,
    array,
    arrayV,
    asVector,
    safeArray,
    unsafeModifyVector,
    validate,
    (><),
  )
import Harpie.Array.Generic qualified as G
import Harpie.Shape qualified as S
import Prelude hiding (cycle, drop, length, repeat, take, zip, zipWith)

type Array a = G.Array VU.Vector a

-- | Conversion to and from a unboxed 'VU.Vector'.
class (VU.Unbox a) => FromVector t a | t -> a where
  asVector :: t -> VU.Vector a
  vectorAs :: VU.Vector a -> t

instance (VU.Unbox a) => FromVector (VU.Vector a) a where
  asVector = id
  vectorAs = id

instance (VU.Unbox a) => FromVector [a] a where
  asVector = VU.fromList
  vectorAs = VU.toList

instance (VU.Unbox a) => FromVector (Array a) a where
  asVector (G.UnsafeArray _ _ v) = v
  vectorAs v = G.unsafeArrayL [VU.length v] v

-- | Conversion to and from an 'Array'.
class (VU.Unbox a) => FromArray t a | t -> a where
  asArray :: t -> Array a
  arrayAs :: Array a -> t

instance (VU.Unbox a) => FromArray (Array a) a where
  asArray = id
  arrayAs = id

instance (VU.Unbox a) => FromArray [a] a where
  asArray l = G.unsafeArrayL [S.rankL l] (VU.fromList l)
  arrayAs (G.UnsafeArray _ _ v) = VU.toList v

instance (VU.Unbox a) => FromArray (VU.Vector a) a where
  asArray v = G.unsafeArrayL [VU.length v] v
  arrayAs (G.UnsafeArray _ _ v) = v

-- | Construct an array from a shape and a value without any shape validation.
array :: (FromVector t a) => [Int] -> t -> Array a
array s (asVector -> v) = G.unsafeArrayL s v

infixl 4 ><

-- | Construct an Array.
(><) :: (FromVector t a) => [Int] -> t -> Array a
(><) = array

-- | Validate the size and shape of an array.
validate :: (VU.Unbox a) => Array a -> Bool
validate a = G.size a == VU.length (asVector a)

-- | Construct an Array, checking shape.
safeArray :: (FromVector t a) => [Int] -> t -> Maybe (Array a)
safeArray s v =
  bool Nothing (Just a) (validate a)
  where
    a = array s v

-- | Unsafely modify an array vector.
unsafeModifyVector :: (FromVector u a, FromVector v b) => (u -> v) -> Array a -> Array b
unsafeModifyVector f (G.UnsafeArray s _ v) = G.unsafeArray s (asVector (f (vectorAs v)))

unsafeArray :: VU.Vector Int -> VU.Vector a -> Array a
unsafeArray = G.unsafeArray

unsafeArrayL :: [Int] -> VU.Vector a -> Array a
unsafeArrayL = G.unsafeArrayL

unsafeModifyShape :: (VU.Vector Int -> VU.Vector Int) -> Array a -> Array a
unsafeModifyShape = G.unsafeModifyShape

tabulate :: (VG.Vector VU.Vector a) => [Int] -> ([Int] -> a) -> Array a
tabulate = G.tabulate

backpermute :: (VG.Vector VU.Vector a) => (VU.Vector Int -> VU.Vector Int) -> (VU.Vector Int -> VU.Vector Int) -> Array a -> Array a
backpermute = G.backpermute

toScalar :: (VG.Vector VU.Vector a) => a -> Array a
toScalar = G.toScalar

asSingleton :: Array a -> Array a
asSingleton = G.asSingleton

asScalar :: Array a -> Array a
asScalar = G.asScalar

empty :: (VG.Vector VU.Vector a) => Array a
empty = G.empty

range :: (VG.Vector VU.Vector Int) => [Int] -> Array Int
range = G.range

corange :: (VG.Vector VU.Vector Int) => [Int] -> Array Int
corange = G.corange

indices :: (VG.Vector VU.Vector [Int]) => [Int] -> Array [Int]
indices = G.indices

ident :: (Num a, VG.Vector VU.Vector a) => [Int] -> Array a
ident = G.ident

konst :: (VG.Vector VU.Vector a) => [Int] -> a -> Array a
konst = G.konst

singleton :: (VG.Vector VU.Vector a) => a -> Array a
singleton = G.singleton

zipWith :: (VG.Vector VU.Vector a, VG.Vector VU.Vector b, VG.Vector VU.Vector c) => (a -> b -> c) -> Array a -> Array b -> Array c
zipWith = G.zipWith

modify :: (VG.Vector VU.Vector a) => [Int] -> (a -> a) -> Array a -> Array a
modify = G.modify

rowWise :: (Dims -> [x] -> Array a -> Array a) -> [x] -> Array a -> Array a
rowWise = G.rowWise

colWise :: (Dims -> [x] -> Array a -> Array a) -> [x] -> Array a -> Array a
colWise = G.colWise

dimsWise :: (Dim -> x -> Array a -> Array a) -> Dims -> [x] -> Array a -> Array a
dimsWise = G.dimsWise

couple :: (VG.Vector VU.Vector a) => Int -> Array a -> Array a -> Array a
couple = G.couple

indexes :: (VG.Vector VU.Vector a) => Dims -> [Int] -> Array a -> Array a
indexes = G.indexes

slices :: (VG.Vector VU.Vector a) => Dims -> [Int] -> [Int] -> Array a -> Array a
slices = G.slices

heads :: (VG.Vector VU.Vector a) => Dims -> Array a -> Array a
heads = G.heads

lasts :: (VG.Vector VU.Vector a) => Dims -> Array a -> Array a
lasts = G.lasts

tails :: (VG.Vector VU.Vector a) => Dims -> Array a -> Array a
tails = G.tails

inits :: (VG.Vector VU.Vector a) => Dims -> Array a -> Array a
inits = G.inits

modifies :: (VG.Vector VU.Vector a, VG.Vector VU.Vector (Array a)) => (Array a -> Array a) -> Dims -> [Int] -> Array a -> Array a
modifies = G.modifies

diffs :: (VG.Vector VU.Vector a, VG.Vector VU.Vector b, VG.Vector VU.Vector (Array a), VG.Vector VU.Vector (Array b)) => Dims -> [Int] -> (Array a -> Array a -> Array b) -> Array a -> Array b
diffs = G.diffs

windows :: (VG.Vector VU.Vector a) => [Int] -> Array a -> Array a
windows = G.windows

find :: (Eq (VU.Vector a), VG.Vector VU.Vector Bool, VG.Vector VU.Vector a, VG.Vector VU.Vector (Array a)) => Array a -> Array a -> Array Bool
find = G.find

findNoOverlap :: (Eq (VU.Vector a), VG.Vector VU.Vector Bool, VG.Vector VU.Vector a, VG.Vector VU.Vector (Array a)) => Array a -> Array a -> Array Bool
findNoOverlap = G.findNoOverlap

findIndices :: (Eq (VU.Vector a), VG.Vector VU.Vector [Int], VG.Vector VU.Vector a, VG.Vector VU.Vector Bool, VG.Vector VU.Vector ([Int], Bool), VG.Vector VU.Vector (Array a)) => Array a -> Array a -> Array [Int]
findIndices = G.findIndices

fill :: (VG.Vector VU.Vector a) => a -> Array a -> Array a
fill = G.fill

flat :: Array a -> Array a
flat = G.flat

rerank :: Int -> Array a -> Array a
rerank = G.rerank

transpose :: (VG.Vector VU.Vector a) => Array a -> Array a
transpose = G.transpose

intercalate :: (VG.Vector VU.Vector a, VG.Vector VU.Vector (Array a)) => Dim -> Array a -> Array a -> Array a
intercalate = G.intercalate

intersperse :: (VG.Vector VU.Vector a, VG.Vector VU.Vector (Array a)) => Dim -> a -> Array a -> Array a
intersperse = G.intersperse

sorts :: (Ord (VU.Vector a), VG.Vector VU.Vector a, VG.Vector VU.Vector Int, VG.Vector VU.Vector (Array a)) => Dims -> Array a -> Array a
sorts = G.sorts

sortsBy :: (Ord (VU.Vector b), VG.Vector VU.Vector a, VG.Vector VU.Vector Int, VG.Vector VU.Vector (Array a)) => Dims -> (Array a -> Array b) -> Array a -> Array a
sortsBy = G.sortsBy

orders :: (Ord (VU.Vector a), VG.Vector VU.Vector Int, VG.Vector VU.Vector a, VG.Vector VU.Vector (Array a)) => Dims -> Array a -> Array Int
orders = G.orders

ordersBy :: (Ord (VU.Vector b), VG.Vector VU.Vector Int, VG.Vector VU.Vector a, VG.Vector VU.Vector (Array a)) => Dims -> (Array a -> Array b) -> Array a -> Array Int
ordersBy = G.ordersBy

transmit :: (VG.Vector VU.Vector b, VG.Vector VU.Vector c, VG.Vector VU.Vector (Array b), VG.Vector VU.Vector (Array c)) => (Array a -> Array b -> Array c) -> Array a -> Array b -> Array c
transmit = G.transmit

transmitOp :: (VG.Vector VU.Vector a, VG.Vector VU.Vector b, VG.Vector VU.Vector c, VG.Vector VU.Vector (Array a), VG.Vector VU.Vector (Array b), VG.Vector VU.Vector (Array c)) => (a -> b -> c) -> Array a -> Array b -> Array c
transmitOp = G.transmitOp

telecasts :: (VG.Vector VU.Vector a, VG.Vector VU.Vector b, VG.Vector VU.Vector c, VG.Vector VU.Vector (Array a), VG.Vector VU.Vector (Array b), VG.Vector VU.Vector (Array c)) => Dims -> Dims -> (Array a -> Array b -> Array c) -> Array a -> Array b -> Array c
telecasts = G.telecasts

cons :: (VG.Vector VU.Vector a) => Array a -> Array a -> Array a
cons = G.cons

snoc :: (VG.Vector VU.Vector a) => Array a -> Array a -> Array a
snoc = G.snoc

iota :: (VG.Vector VU.Vector Int) => Int -> Array Int
iota = G.iota

invtri :: forall a v. (Fractional a, VG.Vector VU.Vector a, VG.Vector VU.Vector Int, VG.Vector VU.Vector (Array a)) => Array a -> Array a
invtri = G.invtri

inverse :: (Floating a, VG.Vector VU.Vector a, VG.Vector VU.Vector Int, VG.Vector VU.Vector (Array a)) => Array a -> Array a
inverse = G.inverse

chol :: (Floating a, VG.Vector VU.Vector a) => Array a -> Array a
chol = G.chol
