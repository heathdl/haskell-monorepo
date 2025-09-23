module Sequence.PerfectNumbers where

import Sequence.Mersenne

perfectNumbers :: (Integral a) => [a]
perfectNumbers = map (\n -> (n * (n + 1)) `div` 2) mersennePrimes

main :: IO ()
main = do
  print perfectNumbers