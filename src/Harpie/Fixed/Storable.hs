{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Arrays with type-level shape information, backed by storable vectors.
--
-- This module is a facade over 'Harpie.Fixed.Generic' with the backing
-- vector family fixed to 'Data.Vector.Storable.Vector'.
module Harpie.Fixed.Storable
  ( -- * Re-exports from the generic core
    module Harpie.Fixed.Generic,

    -- * Storable facade
    Array,
    FromVector (..),
    array,
    safeArray,
    unsafeArray,
    validate,
    unsafeModifyVector,
    vector,
    vector',
  )
where

import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.Vector.Storable (Storable)
import Data.Vector.Storable qualified as VS
import GHC.TypeNats (KnownNat, SNat, withKnownNat)
import Harpie.Fixed.Generic hiding
  ( Array,
    FromVector (..),
    array,
    safeArray,
    unsafeArray,
    unsafeModifyVector,
    validate,
    vector,
    vector',
  )
import Harpie.Fixed.Generic qualified as G
import Harpie.Shape (KnownNats)
import Prelude hiding (cycle, drop, length, repeat, take, zipWith)

-- | A fixed-shape array backed by a storable vector.
type Array s a = G.Array VS.Vector s a

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

instance (Storable a, KnownNats s) => FromVector (Array s a) a where
  asVector (G.Array v) = v
  vectorAs v = G.Array v

-- | Construct an array without shape validation.
unsafeArray :: (KnownNats s, FromVector t a) => t -> Array s a
unsafeArray (asVector -> v) = G.Array v

-- | Validate the size and shape of an array.
validate :: (KnownNats s, Storable a) => Array s a -> Bool
validate a = G.size a == VS.length (asVector a)

-- | Construct an Array, checking shape.
safeArray :: (KnownNats s, FromVector t a) => t -> Maybe (Array s a)
safeArray v =
  bool Nothing (Just a) (validate a)
  where
    a = unsafeArray v

-- | Construct an Array, throwing an exception on a bad shape.
array :: forall s a t. (KnownNats s, FromVector t a) => t -> Array s a
array v =
  fromMaybe (error "Shape Mismatch") (safeArray v)

-- | Unsafely modify an array vector.
unsafeModifyVector ::
  (KnownNats s, KnownNats s', Storable a, Storable b) =>
  (VS.Vector a -> VS.Vector b) ->
  Array s a ->
  Array s' b
unsafeModifyVector f (G.Array v) = G.Array (f v)

-- | Create a one-dimensional array.
vector :: forall n a t. (FromVector t a, KnownNat n) => t -> Array '[n] a
vector xs = array xs

-- | Create a one-dimensional array with an explicit 'SNat'.
vector' :: forall a n t. (FromVector t a) => SNat n -> t -> Array '[n] a
vector' n xs = withKnownNat n (vector xs)
