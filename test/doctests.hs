module Main where

import System.Environment (getArgs)
import Test.DocTest (mainFromCabal)
import Prelude (IO, (=<<))


main :: IO ()
main = mainFromCabal "harry" =<< getArgs

-- (\a -> toDynamic (F.takeBs (Dims @'[0]) (S.SNats @'[1]) a) == (A.takes [0] [-1] (toDynamic a))) (F.range @[2,3,4])
