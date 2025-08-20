module Square (integerSquareRoot, isPerfectSquare) where

import BisectionSearch (bisectionSearch)
import Data.Maybe (fromJust)

integerSquareRoot :: Int -> Int
integerSquareRoot value =
  fromJust (bisectionSearch (\x -> x * x >= value) 1 value)

isPerfectSquare :: Int -> Bool
isPerfectSquare value = sqrt * sqrt == value
  where
    sqrt = integerSquareRoot value