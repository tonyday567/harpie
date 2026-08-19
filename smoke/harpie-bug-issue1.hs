{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Main where

import Data.Functor.Rep (index, tabulate)
import Data.Vector.Unboxed qualified as VU
import Fcf qualified
import Harpie.Fixed qualified as F
import Harpie.Shape qualified as S

-- | bug-issue1
--
-- >>> F.indexes (S.SNats @'[1]) (S.fins @'[3] [1]) (range @[2,3])
main :: IO ()
main = do
  putStrLn $ "using module functions: " <> show x
  putStrLn $ "using inplace functions:" <> show x'
  where
    a = F.range @[2, 3]
    x = F.indexes (S.SNats @'[1]) (S.fins @'[3] [1]) a
    x' = F.indexes (S.SNats @'[1]) (S.fins @'[3] [1]) (range @[2, 3])

range :: forall s. (S.KnownNats s) => F.Array s Int
range = tabulate (S.flatten (VU.fromList (S.valuesOf @s)) . VU.fromList . S.fromFins)

indexes ::
  forall s' s ds xs a.
  ( S.KnownNats s,
    S.KnownNats s',
    s' ~ Fcf.Eval (S.DeleteDims ds s),
    xs ~ Fcf.Eval (S.GetDims ds s)
  ) =>
  F.Dims ds ->
  S.Fins xs ->
  F.Array s a ->
  F.Array s' a
indexes S.SNats xs a = F.unsafeBackpermute (VU.toList . S.insertDims (VU.fromList (S.valuesOf @ds)) (VU.fromList (S.fromFins xs)) . VU.fromList) a

unsafeBackpermute :: forall s' s a. (S.KnownNats s, S.KnownNats s') => ([Int] -> [Int]) -> F.Array s a -> F.Array s' a
unsafeBackpermute f a = tabulate (index a . S.UnsafeFins . f . S.fromFins)
