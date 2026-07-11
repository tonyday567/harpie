-- | Bridge between 'Harpie' arrays and 'hmatrix' for rank-2 arrays.
--
-- This module provides a fast path for matrix multiplication (and future
-- dense linear algebra) by converting rank-2 arrays to 'hmatrix'
-- 'Matrix Double' values, calling BLAS, and converting back.
--
-- The goal is to keep harpie's N-dimensional API while making rank-2
-- operations competitive with hand-written 'hmatrix' code.
module Harpie.Hmatrix
  ( -- * Dynamic storable arrays
    toMatrix,
    fromMatrix,
    multM,

    -- * Fixed storable arrays
    toMatrixF,
    multMF,
  )
where

import Data.Proxy (Proxy (..))
import Data.Vector.Storable qualified as VS
import Data.Vector.Unboxed qualified as VU
import GHC.TypeNats (KnownNat, natVal)
import Harpie.Array.Storable (Array)
import Harpie.Array.Storable qualified as AS
import Harpie.Fixed.Generic qualified as FG
import Harpie.Fixed.Storable qualified as FS
import Numeric.LinearAlgebra
  ( Matrix,
    cols,
    flatten,
    reshape,
    rows,
  )
import Numeric.LinearAlgebra qualified as LA

-- | Convert a rank-2 dynamic storable array to an 'hmatrix' 'Matrix Double'.
--   Returns 'Nothing' if the array is not rank 2.
toMatrix :: Array Double -> Maybe (Matrix Double)
toMatrix (AS.UnsafeArray s _ v)
  | VU.length s == 2 =
      let c = VU.unsafeIndex s 1
       in Just (reshape c (VS.convert v))
  | otherwise = Nothing

-- | Convert an 'hmatrix' 'Matrix Double' to a rank-2 dynamic storable array.
fromMatrix :: Matrix Double -> Array Double
fromMatrix m =
  AS.UnsafeArray
    (VU.fromList [r, c])
    (VU.fromList [c, 1])
    (VS.convert (flatten m))
  where
    r = rows m
    c = cols m

-- | Matrix multiplication with a BLAS fast path for rank-2 dynamic operands.
--
-- If both arguments are rank 2, this calls 'hmatrix' @(<>)@.  Otherwise it
-- falls back to the generic harpie 'mult'.
multM :: Array Double -> Array Double -> Array Double
multM a b =
  case (toMatrix a, toMatrix b) of
    (Just ma, Just mb) -> fromMatrix (ma LA.<> mb)
    _ -> AS.mult a b

-- | Convert a fixed rank-2 storable array to an 'hmatrix' 'Matrix Double'.
toMatrixF ::
  forall m n.
  (KnownNat n) =>
  FS.Array '[m, n] Double ->
  Matrix Double
toMatrixF (FG.Array v) = reshape (fromIntegral (natVal (Proxy :: Proxy n))) (VS.convert v)

-- | Matrix multiplication with a BLAS fast path for fixed rank-2 operands.
multMF ::
  forall m n p.
  (KnownNat n, KnownNat p) =>
  FS.Array '[m, n] Double ->
  FS.Array '[n, p] Double ->
  FS.Array '[m, p] Double
multMF a b = FG.Array (VS.convert (flatten (toMatrixF a LA.<> toMatrixF b)))
