module Sequence.Catalan (catalanNumbers, catalanNumber) where

-- https://oeis.org/A000108
catalanNumbers :: (Integral a) => [a]
catalanNumbers = scanl step 1 [1 ..]
  where
    step previous x = previous * (4 * x - 2) `div` (x + 1)

catalanNumber :: (Integral a) => a -> a
catalanNumber n = binomial (2 * n) n `div` (n + 1)
  where
    binomial n k = product [k + 1 .. n] `div` product [1 .. n - k]

main :: IO ()
main = do
  print (take 15 catalanNumbers)
