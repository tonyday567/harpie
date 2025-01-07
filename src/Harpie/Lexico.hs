-- | Lexicographic routines
module Harpie.Lexico
  ( combinations,
    combinationsR,
    toLexiPosR,
    toLexiPosRList,
    fromLexiPosR,
    binom,
    binomR,
  )
where

import Control.Applicative
import Data.Bool
import Data.Foldable
import qualified Data.List as List
import Data.Ord
import qualified Data.Vector.Storable as S
import GHC.Exts hiding (toList)
import Prelude

-- $setup
--
-- >>> import Poker.Lexico
-- >>> import Data.Word
-- >>> import qualified Data.List as List
-- >>> import qualified Data.Vector.Storable as S

-- | @combinations k xs@ generates a list of k-combinations from xs
--
-- >>> combinations 2 [0..4]
-- [[0,1],[0,2],[0,3],[0,4],[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations m l = [x : ys | x : xs <- List.tails l, ys <- combinations (m - 1) xs]

-- | List of k-element combinations in reverse lexicographic order.
--
-- >>> combinationsR 2 [0..4]
-- [[3,4],[2,4],[1,4],[0,4],[2,3],[1,3],[0,3],[1,2],[0,2],[0,1]]
--
-- > length (combinationsR 5 [0..51]) == binom 52 5
-- True
combinationsR :: Int -> [a] -> [[a]]
combinationsR 0 _ = [[]]
combinationsR m l = reverse <$> combinations m (reverse l)

-- | Given a combination, what is its position in reverse lexicographic ordering of all combinations.
--
--  <https://math.stackexchange.com/questions/1368526/fast-way-to-get-a-combination-given-its-position-in-reverse-lexicographic-or/1368570#1368570 stackexchange question>
--
-- >>> toLexiPosRList 52 2 [50,51]
-- 0
--
-- >>>  toLexiPosRList 52 2 [0,1]
-- 1325
--
-- >>> toLexiPosRList 5 2 <$> combinationsR 2 [0..4]
-- [0,1,2,3,4,5,6,7,8,9]
toLexiPosRList :: Int -> Int -> [Int] -> Int
toLexiPosRList n k xs = binom n k - 1 - sum (zipWith binom xs [1 ..])

-- | Given a reverse lexicographic position, what was the combination?
--
-- >>> (\xs -> xs == fmap (fromLexiPosR 5 2 . toLexiPosRList 5 2) xs) (combinations 2 [0..4])
-- True
--
-- > (fromIntegral <$> (S.toList $ unwrapCardsS $ review cardsI $ combinationsR 5 allCards List.!! 1000000)) == (fromLexiPosR 52 5 1000000)
-- True
fromLexiPosR :: Int -> Int -> Int -> [Int]
fromLexiPosR n k p = go (n - 1) k (binom n k - 1 - p) []
  where
    go n' k' p' xs =
      bool
        ( bool
            (go (n' - 1) k' p' xs)
            (go (n' - 1) (k' - 1) (p' - binom n' k') (n' : xs))
            (p' >= binom n' k')
        )
        xs
        (length xs == k)

-- | binomial equation
--
-- The number of 7-card combinations for a 52 card deck is:
--
-- >>> binom 52 7
-- 133784560
binom :: (Integral a) => a -> a -> a
binom _ 0 = 1
binom 0 _ = 0
binom n k = product [(n - k + 1) .. n] `div` product [1 .. k]

-- | recursive version of binomial equation
--
-- >>> binomR 52 7
-- 133784560
binomR :: (Integral a) => a -> a -> a
binomR _ 0 = 1
binomR 0 _ = 0
binomR n k = binomR (n - 1) (k - 1) * n `div` k

-- | reverse lexicographic position of a storable vector with enumerated binom function
--
-- > toLexiPosR n k s = binom n k - 1 - S.sum (S.imap (\i a -> binom a (1+i)) s)
-- > toLexiPosR == toLexiPosR . S.fromList
--
-- >>> toLexiPosR 5 2 <$> S.fromList <$> combinationsR 2 [(0::Int)..4] :: [Int]
-- [0,1,2,3,4,5,6,7,8,9]
toLexiPosR :: (S.Storable a, S.Storable b, Integral a, Integral b) => a -> a -> S.Vector b -> a
toLexiPosR n k s =
  binom n k - 1 - S.sum (S.imap (\i a -> fromIntegral $ binom (fromIntegral a) (1 + i)) s)
