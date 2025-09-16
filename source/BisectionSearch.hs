module BisectionSearch (bisectionSearch) where

import Data.Maybe (fromJust)

bisectionSearch :: (Int -> Bool) -> Int -> Int -> Maybe Int
bisectionSearch predicate low high
  | low > high = Nothing
  | otherwise = go low high
  where
    go low high
      | low < high && predicate middle = go low middle
      | low < high = go (middle + 1) high
      | predicate low = Just low
      | otherwise = Nothing
      where
        middle = (low + high) `div` 2
