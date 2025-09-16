module Sequence.Catalan (catalanNumbers) where

catalanNumbers :: (Integral a) => [a]
catalanNumbers = scanl step 1 [1 ..]
  where
    step previous x = previous * (4 * x - 2) `div` (x + 1)

main :: IO ()
main = do
  print (take 15 catalanNumbers)