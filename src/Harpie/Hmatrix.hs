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
import Data.Vector.Unboxed qualified as VU
import Harpie.Array (Array (..), mult)
import Numeric.LinearAlgebra
  ( Matrix,
    cols,
    flatten,
    fromList,
    reshape,
    rows,
    toList,
  )
import Numeric.LinearAlgebra qualified as LA
import Prelude hiding ((<>))

-- | Convert a rank-2 'Array Double' to an 'hmatrix' 'Matrix Double'.
--   Returns 'Nothing' if the array is not rank 2.
toMatrix :: Array Double -> Maybe (Matrix Double)
toMatrix (UnsafeArray s _ v) =
  case VU.toList s of
    [_, c] -> Just (reshape c (fromList (V.toList v)))
    _ -> Nothing

-- | Convert an 'hmatrix' 'Matrix Double' to a rank-2 'Array Double'.
fromMatrix :: Matrix Double -> Array Double
fromMatrix m =
  UnsafeArray (VU.fromList [rows m, cols m]) (VU.fromList (strides [rows m, cols m])) (V.fromList (toList (flatten m)))
  where
    strides sh = drop 1 (scanr (*) 1 sh)

-- | Matrix multiplication with a BLAS fast path for rank-2 operands.
--
-- If both arguments are rank 2, this calls 'hmatrix' @(<>)@.  Otherwise it
-- falls back to the generic harpie 'mult'.
multM :: Array Double -> Array Double -> Array Double
multM a b =
  case (toMatrix a, toMatrix b) of
    (Just ma, Just mb) -> fromMatrix (ma LA.<> mb)
    _ -> mult a b
