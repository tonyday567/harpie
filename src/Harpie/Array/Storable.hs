{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

-- | Arrays with shape information and computations at a value-level.
--
-- This module is a storable-vector facade over 'Harpie.Array.Generic'.
module Harpie.Array.Storable
  ( -- * Re-exports from the generic core
    module Harpie.Array.Generic,

    -- * Storable facade
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
import Data.Vector.Storable (Storable, Vector)
import Data.Vector.Storable qualified as VS
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

type Array a = G.Array VS.Vector a

-- | Conversion to and from a storable 'VS.Vector'.
class (Storable a) => FromVector t a | t -> a where
  asVector :: t -> VS.Vector a
  vectorAs :: VS.Vector a -> t

instance (Storable a) => FromVector (VS.Vector a) a where
  asVector = id
  vectorAs = id

instance (Storable a) => FromVector [a] a where
  asVector = VS.fromList
  vectorAs = VS.toList

instance (Storable a) => FromVector (Array a) a where
  asVector (G.UnsafeArray _ _ v) = v
  vectorAs v = G.unsafeArrayL [VS.length v] v

-- | Conversion to and from an 'Array'.
class (Storable a) => FromArray t a | t -> a where
  asArray :: t -> Array a
  arrayAs :: Array a -> t

instance (Storable a) => FromArray (Array a) a where
  asArray = id
  arrayAs = id

instance (Storable a) => FromArray [a] a where
  asArray l = G.unsafeArrayL [S.rankL l] (VS.fromList l)
  arrayAs (G.UnsafeArray _ _ v) = VS.toList v

instance (Storable a) => FromArray (VS.Vector a) a where
  asArray v = G.unsafeArrayL [VS.length v] v
  arrayAs (G.UnsafeArray _ _ v) = v

-- | Construct an array from a shape and a value without any shape validation.
array :: (FromVector t a) => [Int] -> t -> Array a
array s (asVector -> v) = G.unsafeArrayL s v

infixl 4 ><

-- | Construct an Array.
(><) :: (FromVector t a) => [Int] -> t -> Array a
(><) = array

-- | Validate the size and shape of an array.
validate :: (Storable a) => Array a -> Bool
validate a = G.size a == VS.length (asVector a)

-- | Construct an Array, checking shape.
safeArray :: (FromVector t a) => [Int] -> t -> Maybe (Array a)
safeArray s v =
  bool Nothing (Just a) (validate a)
  where
    a = array s v

-- | Unsafely modify an array vector.
unsafeModifyVector :: (FromVector u a, FromVector v b) => (u -> v) -> Array a -> Array b
unsafeModifyVector f (G.UnsafeArray s _ v) = G.unsafeArray s (asVector (f (vectorAs v)))

unsafeArray :: VU.Vector Int -> VS.Vector a -> Array a
unsafeArray = G.unsafeArray

unsafeArrayL :: [Int] -> VS.Vector a -> Array a
unsafeArrayL = G.unsafeArrayL

unsafeModifyShape :: (VU.Vector Int -> VU.Vector Int) -> Array a -> Array a
unsafeModifyShape = G.unsafeModifyShape

tabulate :: (VG.Vector VS.Vector a) => [Int] -> ([Int] -> a) -> Array a
tabulate = G.tabulate

backpermute :: (VG.Vector VS.Vector a) => (VU.Vector Int -> VU.Vector Int) -> (VU.Vector Int -> VU.Vector Int) -> Array a -> Array a
backpermute = G.backpermute

toScalar :: (VG.Vector VS.Vector a) => a -> Array a
toScalar = G.toScalar

asSingleton :: Array a -> Array a
asSingleton = G.asSingleton

asScalar :: Array a -> Array a
asScalar = G.asScalar

empty :: (VG.Vector VS.Vector a) => Array a
empty = G.empty

range :: (VG.Vector VS.Vector Int) => [Int] -> Array Int
range = G.range

corange :: (VG.Vector VS.Vector Int) => [Int] -> Array Int
corange = G.corange

indices :: (VG.Vector VS.Vector [Int]) => [Int] -> Array [Int]
indices = G.indices

ident :: (Num a, VG.Vector VS.Vector a) => [Int] -> Array a
ident = G.ident

konst :: (VG.Vector VS.Vector a) => [Int] -> a -> Array a
konst = G.konst

singleton :: (VG.Vector VS.Vector a) => a -> Array a
singleton = G.singleton

zipWith :: (VG.Vector VS.Vector a, VG.Vector VS.Vector b, VG.Vector VS.Vector c) => (a -> b -> c) -> Array a -> Array b -> Array c
zipWith = G.zipWith

modify :: (VG.Vector VS.Vector a) => [Int] -> (a -> a) -> Array a -> Array a
modify = G.modify

rowWise :: (Dims -> [x] -> Array a -> Array a) -> [x] -> Array a -> Array a
rowWise = G.rowWise

colWise :: (Dims -> [x] -> Array a -> Array a) -> [x] -> Array a -> Array a
colWise = G.colWise

dimsWise :: (Dim -> x -> Array a -> Array a) -> Dims -> [x] -> Array a -> Array a
dimsWise = G.dimsWise

couple :: (VG.Vector VS.Vector a) => Int -> Array a -> Array a -> Array a
couple = G.couple

indexes :: (VG.Vector VS.Vector a) => Dims -> [Int] -> Array a -> Array a
indexes = G.indexes

slices :: (VG.Vector VS.Vector a) => Dims -> [Int] -> [Int] -> Array a -> Array a
slices = G.slices

heads :: (VG.Vector VS.Vector a) => Dims -> Array a -> Array a
heads = G.heads

lasts :: (VG.Vector VS.Vector a) => Dims -> Array a -> Array a
lasts = G.lasts

tails :: (VG.Vector VS.Vector a) => Dims -> Array a -> Array a
tails = G.tails

inits :: (VG.Vector VS.Vector a) => Dims -> Array a -> Array a
inits = G.inits

modifies :: (VG.Vector VS.Vector a, VG.Vector VS.Vector (Array a)) => (Array a -> Array a) -> Dims -> [Int] -> Array a -> Array a
modifies = G.modifies

diffs :: (VG.Vector VS.Vector a, VG.Vector VS.Vector b, VG.Vector VS.Vector (Array a), VG.Vector VS.Vector (Array b)) => Dims -> [Int] -> (Array a -> Array a -> Array b) -> Array a -> Array b
diffs = G.diffs

windows :: (VG.Vector VS.Vector a) => [Int] -> Array a -> Array a
windows = G.windows

find :: (Eq (VS.Vector a), VG.Vector VS.Vector Bool, VG.Vector VS.Vector a, VG.Vector VS.Vector (Array a)) => Array a -> Array a -> Array Bool
find = G.find

findNoOverlap :: (Eq (VS.Vector a), VG.Vector VS.Vector Bool, VG.Vector VS.Vector a, VG.Vector VS.Vector (Array a)) => Array a -> Array a -> Array Bool
findNoOverlap = G.findNoOverlap

findIndices :: (Eq (VS.Vector a), VG.Vector VS.Vector [Int], VG.Vector VS.Vector a, VG.Vector VS.Vector Bool, VG.Vector VS.Vector ([Int], Bool), VG.Vector VS.Vector (Array a)) => Array a -> Array a -> Array [Int]
findIndices = G.findIndices

fill :: (VG.Vector VS.Vector a) => a -> Array a -> Array a
fill = G.fill

flat :: Array a -> Array a
flat = G.flat

rerank :: Int -> Array a -> Array a
rerank = G.rerank

transpose :: (VG.Vector VS.Vector a) => Array a -> Array a
transpose = G.transpose

intercalate :: (VG.Vector VS.Vector a, VG.Vector VS.Vector (Array a)) => Dim -> Array a -> Array a -> Array a
intercalate = G.intercalate

intersperse :: (VG.Vector VS.Vector a, VG.Vector VS.Vector (Array a)) => Dim -> a -> Array a -> Array a
intersperse = G.intersperse

sorts :: (Ord (VS.Vector a), VG.Vector VS.Vector a, VG.Vector VS.Vector Int, VG.Vector VS.Vector (Array a)) => Dims -> Array a -> Array a
sorts = G.sorts

sortsBy :: (Ord (VS.Vector b), VG.Vector VS.Vector a, VG.Vector VS.Vector Int, VG.Vector VS.Vector (Array a)) => Dims -> (Array a -> Array b) -> Array a -> Array a
sortsBy = G.sortsBy

orders :: (Ord (VS.Vector a), VG.Vector VS.Vector Int, VG.Vector VS.Vector a, VG.Vector VS.Vector (Array a)) => Dims -> Array a -> Array Int
orders = G.orders

ordersBy :: (Ord (VS.Vector b), VG.Vector VS.Vector Int, VG.Vector VS.Vector a, VG.Vector VS.Vector (Array a)) => Dims -> (Array a -> Array b) -> Array a -> Array Int
ordersBy = G.ordersBy

transmit :: (VG.Vector VS.Vector b, VG.Vector VS.Vector c, VG.Vector VS.Vector (Array b), VG.Vector VS.Vector (Array c)) => (Array a -> Array b -> Array c) -> Array a -> Array b -> Array c
transmit = G.transmit

transmitOp :: (VG.Vector VS.Vector a, VG.Vector VS.Vector b, VG.Vector VS.Vector c, VG.Vector VS.Vector (Array a), VG.Vector VS.Vector (Array b), VG.Vector VS.Vector (Array c)) => (a -> b -> c) -> Array a -> Array b -> Array c
transmitOp = G.transmitOp

telecasts :: (VG.Vector VS.Vector a, VG.Vector VS.Vector b, VG.Vector VS.Vector c, VG.Vector VS.Vector (Array a), VG.Vector VS.Vector (Array b), VG.Vector VS.Vector (Array c)) => Dims -> Dims -> (Array a -> Array b -> Array c) -> Array a -> Array b -> Array c
telecasts = G.telecasts

cons :: (VG.Vector VS.Vector a) => Array a -> Array a -> Array a
cons = G.cons

snoc :: (VG.Vector VS.Vector a) => Array a -> Array a -> Array a
snoc = G.snoc

iota :: (VG.Vector VS.Vector Int) => Int -> Array Int
iota = G.iota

invtri :: forall a v. (Fractional a, VG.Vector VS.Vector a, VG.Vector VS.Vector Int, VG.Vector VS.Vector (Array a)) => Array a -> Array a
invtri = G.invtri

inverse :: (Floating a, VG.Vector VS.Vector a, VG.Vector VS.Vector Int, VG.Vector VS.Vector (Array a)) => Array a -> Array a
inverse = G.inverse

chol :: (Floating a, VG.Vector VS.Vector a) => Array a -> Array a
chol = G.chol
