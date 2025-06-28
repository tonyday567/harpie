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
  print $ simp @'[2] (SNats @'[2])

simp ::
  forall ds.
  ( -- KnownNats s
    -- s' ~ Fcf.Eval (S.DeleteDims ds s),
    -- xs ~ Fcf.Eval (S.GetDims ds s)
  ) =>
  SNats ds ->
  [Int]
simp (SNats :: SNats ds) = valuesOf @ds

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

valuesOf :: forall s. (KnownNats s) => [Int]
valuesOf = fmap fromIntegral (fromSNats (SNats :: SNats s))

-- | Matches GHC printing quirks.
instance Show (SNats ns) where
  show (UnsafeSNats s) = "SNats @" <> bool "" "'" (length s < 2) <> "[" <> mconcat (List.intersperse ", " (show <$> s)) <> "]"
