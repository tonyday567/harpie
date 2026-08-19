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
