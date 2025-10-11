module Sequence.Sum (sumOfIntegers, sumOfIntegersResidueClass) where

sumOfIntegers :: (Integral a) => a -> a
sumOfIntegers n = n * (n + 1) `div` 2

sumOfIntegersResidueClass :: (Integral a) => a -> a -> a -> a
sumOfIntegersResidueClass 1 2 n = n ^ 2
sumOfIntegersResidueClass 0 m n = m * sumOfIntegers n
sumOfIntegersResidueClass r m n = m * sumOfIntegers n - r * n

main :: IO ()
main = do
  print (sumOfIntegers 5)
  print (sumOfIntegersResidueClass 1 2 4)
