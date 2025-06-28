{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Functor.Classes
import Data.Functor.Rep
import Data.Bool
import Data.List qualified as List
import Data.Vector qualified as V
import GHC.TypeNats
import GHC.Generics
import Data.Distributive
import GHC.Exts
import Data.Maybe
import Prelude as P

-- | bug-issue1 intermediate results
--
-- >>> index (range @[2,3]) (UnsafeFins [0,1])
-- 1
--
-- >>> index (range @[2,3]) (UnsafeFins [1,1])
-- 4
--
-- >>> UnsafeFins [1]
-- [1]
--
-- >>> SNats @'[1]
-- SNats @'[1]
--
-- >>> index (indexes @'[2] (SNats @'[1]) (UnsafeFins [1]) (range @[2,3])) (UnsafeFins [0])
-- 1
--
-- >>> unsafeBackpermute @'[2] (insertDims (valuesOf @'[1]) (fromFins (UnsafeFins [1]))) (range @[2,3])
-- [1,4]
--
-- unsafeBackpermute @'[2] (insertDims [1] [1]) (range @[2,3])
-- [1,4]
--
main :: IO ()
main = do
  print $ indexes_ @'[2] [1] (UnsafeFins [1]) (range @[2,3])
  print $ indexesV4 @'[2] (SNats @'[2])
  -- print $ indexesV3 @'[2] (SNats @'[2]) (range @[2,3])
  -- print $ indexesV2 @'[2] (SNats @'[1]) (UnsafeFins [1]) (range @[2,3])
  -- print $ indexes @'[2] (SNats @'[1]) (UnsafeFins [1]) (range @[2,3])

range :: forall s. (KnownNats s) => Array s Int
range = tabulate (flatten (valuesOf @s) . fromFins)

indexes ::
  forall s' s ds xs a.
  ( KnownNats s,
    KnownNats s'
    -- s' ~ Fcf.Eval (S.DeleteDims ds s),
    -- xs ~ Fcf.Eval (S.GetDims ds s)
  ) =>
  SNats ds ->
  Fins xs ->
  Array s a ->
  Array s' a
indexes (SNats :: SNats ds) xs a = unsafeBackpermute (insertDims (valuesOf @ds) (fromFins xs)) a
{-# inline indexes #-}

indexes_ ::
  forall s' s xs a.
  ( KnownNats s,
    KnownNats s'
    -- s' ~ Fcf.Eval (S.DeleteDims ds s),
    -- xs ~ Fcf.Eval (S.GetDims ds s)
  ) =>
  [Int] ->
  -- SNats ds ->
  Fins xs ->
  Array s a ->
  Array s' a
indexes_ ds xs a = unsafeBackpermute (insertDims ds (fromFins xs)) a

indexesV2 ::
  forall s' s ds xs a.
  ( KnownNats s,
    KnownNats s'
    -- s' ~ Fcf.Eval (S.DeleteDims ds s),
    -- xs ~ Fcf.Eval (S.GetDims ds s)
  ) =>
  SNats ds ->
  Fins xs ->
  Array s a ->
  [Int]
indexesV2 (SNats :: SNats ds) xs a = valuesOf @ds

indexesV3 ::
  forall s' s ds a.
  ( KnownNats s,
    KnownNats s'
    -- s' ~ Fcf.Eval (S.DeleteDims ds s),
    -- xs ~ Fcf.Eval (S.GetDims ds s)
  ) =>
  SNats ds ->
  Array s a ->
  [Int]
indexesV3 (SNats :: SNats ds) a = valuesOf @ds

indexesV4 ::
  forall ds.
  ( -- KnownNats s
    -- s' ~ Fcf.Eval (S.DeleteDims ds s),
    -- xs ~ Fcf.Eval (S.GetDims ds s)
  ) =>
  SNats ds ->
  [Int]
indexesV4 (SNats :: SNats ds) = valuesOf @ds

unsafeBackpermute :: forall s' s a. (KnownNats s, KnownNats s') => ([Int] -> [Int]) -> Array s a -> Array s' a
unsafeBackpermute f a = tabulate (index a . UnsafeFins . f . fromFins)

type role Array nominal representational

newtype Array (s :: [Nat]) a where
  Array :: V.Vector a -> Array s a
  deriving stock (Functor, Foldable, Generic, Traversable)
  deriving newtype (Eq, Eq1, Ord, Ord1, Show, Show1)

instance
  (KnownNats s) =>
  Data.Distributive.Distributive (Array s)
  where
  distribute :: (Functor f) => f (Array s a) -> Array s (f a)
  distribute = distributeRep
  {-# INLINE distribute #-}

instance
  forall s.
  (KnownNats s) =>
  Representable (Array s)
  where
  type Rep (Array s) = Fins s

  tabulate f =
    Array . V.generate (size s) $ (f . UnsafeFins . shapen s)
    where
      s = valuesOf @s
  {-# INLINE tabulate #-}

  index (Array v) i = V.unsafeIndex v (flatten s (fromFins i))
    where
      s = valuesOf @s
  {-# INLINE index #-}

type Dims = SNats

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

-- | Convert an explicit SNats ns value into an implicit KnownNats ns constraint.
withKnownNats ::
  forall ns rep (r :: TYPE rep).
  SNats ns -> ((KnownNats ns) => r) -> r
withKnownNats = withDict @(KnownNats ns)

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

-- | Convert from a n-dimensional shape list index to a flat index, which, technically is the lexicographic position of the position in a row-major array.
--
-- >>> flatten [2,3,4] [1,1,1]
-- 17
--
-- >>> flatten [] [1,1,1]
-- 0
flatten :: [Int] -> [Int] -> Int
flatten [] _ = 0
flatten _ [x'] = x'
flatten ns xs = sum $ zipWith (*) xs (drop 1 $ scanr (*) 1 ns)
{-# INLINE flatten #-}

-- | Convert from a flat index to a shape index.
--
-- >>> shapen [2,3,4] 17
-- [1,1,1]
shapen :: [Int] -> Int -> [Int]
shapen [] _ = []
shapen [_] x' = [x']
shapen [_, y] x' = let (i, j) = divMod x' y in [i, j]
shapen ns x =
  fst $
    foldr
      ( \a (acc, r) ->
          let (d, m) = divMod r a
           in (m : acc, d)
      )
      ([], x)
      ns
{-# INLINE shapen #-}

insertDims :: [Int] -> [Int] -> [Int] -> [Int]
insertDims ds xs s = foldl' (flip (uncurry insertDim)) s ps
  where
    ps = zip (preInsertPositions ds) xs

preDeletePositions :: [Int] -> [Int]
preDeletePositions as = reverse (go as [])
  where
    go [] r = r
    go (x : xs) r = go (decPast x <$> xs) (x : r)
    decPast x y = bool (y - 1) y (y < x)

preInsertPositions :: [Int] -> [Int]
preInsertPositions = reverse . preDeletePositions . reverse

insertDim :: Int -> Int -> [Int] -> [Int]
insertDim d i s = take d s ++ (i : drop d s)

type role Fins nominal

newtype Fins s
  = UnsafeFins
  { fromFins :: [Int]
  }
  deriving stock (Eq, Ord, Functor)

valuesOf :: forall s. (KnownNats s) => [Int]
valuesOf = fmap fromIntegral (fromSNats (SNats :: SNats s))

fins :: forall s. (KnownNats s) => [Int] -> Fins s
fins x = fromMaybe (error "value outside bounds") (safeFins x)

safeFins :: forall s. (KnownNats s) => [Int] -> Maybe (Fins s)
safeFins xs = bool Nothing (Just (UnsafeFins xs)) (isFins xs (valuesOf @s))

isFins :: [Int] -> [Int] -> Bool
isFins xs ds = length xs == length ds && and (zipWith isFin xs ds)

isFin :: Int -> Int -> Bool
isFin i d = 0 <= i && i + 1 <= d

size :: [Int] -> Int
size [] = 1
size [x] = x
size xs = P.product xs

instance Show (Fins n) where
  show (UnsafeFins x) = show x

-- | Matches GHC printing quirks.
instance Show (SNats ns) where
  show (UnsafeSNats s) = "SNats @" <> bool "" "'" (length s < 2) <> "[" <> mconcat (List.intersperse ", " (show <$> s)) <> "]"
