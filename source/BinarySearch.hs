module BinarySearch where

import BisectionSearch (bisectionSearch)

binarySearch :: [Int] -> Int -> Maybe Int
binarySearch [] _ = Nothing
binarySearch list value
  | value < head list = Nothing
  | otherwise = bisectionSearch (\i -> list !! i >= value) 0 (length list - 1)
