module Square (integerSquareRoot, isPerfectSquare, newtonRaphsonSquareRoot) where

import BisectionSearch (bisectionSearch)
import Data.Maybe (fromJust)
import Debug.Trace (trace)

integerSquareRoot :: (Integral a) => a -> a
integerSquareRoot value = go value
  where
    go x
      | x' == x = x'
      | otherwise = go x'
      where
        x' = (x * x + value) `div` (2 * x)

isPerfectSquare :: (Integral a) => a -> Bool
isPerfectSquare value = root * root == value
  where
    root = integerSquareRoot value

newtonRaphsonSquareRoot :: Rational -> Int -> Rational
newtonRaphsonSquareRoot value = go value
  where
    go x 0 = x
    go x rounds = 0.5 * (x' + value / x')
      where
        x' = go x (rounds - 1)

main :: IO ()
main = do
  print (isPerfectSquare (123456789 ^ 2))
  print (isPerfectSquare (123456789 ^ 2 + 1))
  print (isPerfectSquare (123456790 ^ 2))
