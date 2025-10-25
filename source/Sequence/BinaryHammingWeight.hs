module Sequence.BinaryHammingWeight where

import Data.Bits (Bits (shiftR, xor, (.&.), (.|.)))
import Merge (mergeStrictlyIncreasing)

iterativeHakmem1750 :: Int -> [Word]
iterativeHakmem1750 n = iterate next (2 ^ n - 1)
  where
    next x = r .|. o'
      where
        s = x .&. negate x
        r = x + s
        o = r `xor` x
        o' = (o `shiftR` 2) `div` s

binaryHammingWeightOf :: (Integral a) => Int -> [a]
binaryHammingWeightOf 0 = [0]
binaryHammingWeightOf 1 = iterate (* 2) 1 -- not required, presumably improves performance
binaryHammingWeightOf n = go (2 ^ (n - 1))
  where
    go offset = map (offset +) (takeWhile (< offset) previous) ++ go (offset * 2)
    previous = binaryHammingWeightOf (n - 1)

chooseNOfK :: (Integral a) => a -> a -> [[a]]
chooseNOfK 0 _ = [[]]
chooseNOfK n k
  | n <= k = [x : xs | x <- [0 .. k - 1], xs <- chooseNOfK (n - 1) x]
  | otherwise = []

binaryHammingWeightWithResidue :: (Integral a) => Int -> Int -> [a]
binaryHammingWeightWithResidue m n = prefix ++ mergeStrictlyIncreasing rest
  where
    (start, prefix)
      | n == 0 = (m, [0])
      | otherwise = (n, [])
    rest = [binaryHammingWeightOf (start + m * k) | k <- [0 ..]]

main :: IO ()
main = do
  print (take 64 (binaryHammingWeightWithResidue 2 0))