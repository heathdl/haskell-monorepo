module Sequence.BinaryHammingWeight where

import BinaryDisplay (showAutoJustifiedBinaryList, showBinaryList, showJustifiedBinaryList)
import BinaryDisplay.DisplayTypes (redColour)
import Data.Bits (Bits (shiftR, xor, (.&.), (.|.)))

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

main :: IO ()
main = do
  putStrLn (showJustifiedBinaryList redColour k bitmaps)
  print bitmaps
  print (map (foldr (\x -> (+) (2 ^ x)) 0) (chooseNOfK n k))
  where
    n = 4
    k = 10
    bitmaps = takeWhile (<= 2 ^ k) (binaryHammingWeightOf n) :: [Integer]
