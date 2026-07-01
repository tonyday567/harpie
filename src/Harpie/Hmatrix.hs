-- | Bridge between 'Harpie.Array' and 'hmatrix' for rank-2 arrays.
--
-- This module provides a fast path for matrix multiplication (and future
-- dense linear algebra) by converting rank-2 'Array Double' values to
-- 'hmatrix' 'Matrix Double' values, calling BLAS, and converting back.
--
-- The goal is to keep harpie's N-dimensional API while making rank-2
-- operations competitive with hand-written 'hmatrix' code.
module Harpie.Hmatrix
  ( toMatrix,
    fromMatrix,
    multM,
  )
where

import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Vector.Unboxed qualified as VU
import Harpie.Array (Array (..), mult)
import Numeric.LinearAlgebra
  ( Matrix,
    cols,
    flatten,
    reshape,
    rows,
  )
import Numeric.LinearAlgebra qualified as LA

-- | Convert a rank-2 'Array Double' to an 'hmatrix' 'Matrix Double'.
--   Returns 'Nothing' if the array is not rank 2.
toMatrix :: Array Double -> Maybe (Matrix Double)
toMatrix (UnsafeArray s _ v)
  | VU.length s == 2 =
      let c = VU.unsafeIndex s 1
       in Just (reshape c (VS.convert v))
  | otherwise = Nothing

-- | Convert an 'hmatrix' 'Matrix Double' to a rank-2 'Array Double'.
fromMatrix :: Matrix Double -> Array Double
fromMatrix m =
  UnsafeArray
    (VU.fromListN 2 [r, c])
    (VU.fromListN 2 [c, 1])
    (V.convert (flatten m))
  where
    r = rows m
    c = cols m

-- | Matrix multiplication with a BLAS fast path for rank-2 operands.
--
-- If both arguments are rank 2, this calls 'hmatrix' @(<>)@.  Otherwise it
-- falls back to the generic harpie 'mult'.
multM :: Array Double -> Array Double -> Array Double
multM a b =
  case (toMatrix a, toMatrix b) of
    (Just ma, Just mb) -> fromMatrix (ma LA.<> mb)
    _ -> mult a b
