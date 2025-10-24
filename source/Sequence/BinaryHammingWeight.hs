module Sequence.BinaryHammingWeight where

import BinaryDisplay (showAutoJustifiedBinaryList, showBinaryList, showJustifiedBinaryList)
import BinaryDisplay.DisplayTypes (monochrome)
import Data.Bits (Bits (shiftR, xor, (.&.), (.|.)))
import Data.Maybe qualified

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
binaryHammingWeightWithResidue m n = prefix ++ go [binaryHammingWeightOf start] rest (2 ^ start - 1)
  where
    (start, prefix)
      | n == 0 = (m, [0])
      | otherwise = (n, [])
    rest = [binaryHammingWeightOf (start + m * k) | k <- [1 ..]]

    go workingLists untappedLists threshold =
      case extractMinima threshold workingLists of
        (Nothing, _) -> go (head untappedLists : workingLists) (tail untappedLists) threshold'
        (Just minima, remainingLists) -> minima : go remainingLists untappedLists threshold
      where
        threshold' = threshold * (2 ^ m) + (2 ^ m - 1)

    extractMinima _ [] = (Nothing, [])
    extractMinima threshold lists
      | minima >= threshold = (Nothing, lists)
      | otherwise = (Just minima, subsequent)
      where
        heads = map head lists
        minima = minimum heads
        subsequent = zipWith (\xs h -> if h == minima then tail xs else xs) lists heads

main :: IO ()
main = do
  putStrLn (showBinaryList monochrome bitmaps)
  where
    bitmaps = binaryHammingWeightWithResidue 2 1