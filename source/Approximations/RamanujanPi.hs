module Approximations.RamanujanPi where

import Approximations (formattedDigitsOf)
import Approximations.PellRoot2 (pellRationals)
import Data.Bits (Bits (shiftL))
import Data.Ratio (Ratio, denominator, numerator, (%))

ramanujanPi :: [Rational]
ramanujanPi = zipWith (\x y -> (9801 * y) / (4 * x)) ramanujanSum (pellRationals 10)
  where
    ramanujanFactorialTerm = map (\(a, _, _, _) -> a) (iterate go (1, 1, 1, 1))
      where
        go (x, n, n2, n3) = (term, n', n2', n3')
          where
            n' = n + 1
            n2' = n2 + (n `shiftL` 1) + 1
            n3' = n3 + 3 * (n + n2) + 1

            doubN = n `shiftL` 1
            quadN = n `shiftL` 2
            term = ((quadN - 3) * (doubN - 1) * (quadN - 1) * x `shiftL` 3) `div` n3

    ramanujanNumerator = iterate (+ 26390) 1103
    ramanujanDenominator = iterate (* 24591257856) 1

    ramanujanSumTerms = zipWith3 (\a b c -> (a * b) % c) ramanujanFactorialTerm ramanujanNumerator ramanujanDenominator
    ramanujanSum = scanl1 (+) ramanujanSumTerms

main :: IO ()
main = do
  putStrLn (formattedDigitsOf (takeEvery 100 ramanujanPi))
  where
    takeEvery n xs@(x : _) = x : takeEvery n (drop n xs)