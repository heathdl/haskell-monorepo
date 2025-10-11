module Sequence.Carmichael (isCarmichael, isOddCompositeCarmichael, carmichaelNumbers) where

import Composites (oddComposites)
import Prime (isPrime)
import Prime.Coprimes (calculateCoprimes)
import RaiseToPowerModulo (raiseToSomePowerModulo)

isOddCompositeCarmichael :: (Integral a) => a -> Bool
isOddCompositeCarmichael n = all isFalseWitness coprimes
  where
    coprimes = calculateCoprimes n
    isFalseWitness b = raiseToSomePowerModulo b (n - 1) n == 1

isCarmichael :: (Integral a) => a -> Bool
isCarmichael n
  | n < 2 = False
  | isPrime n = False
  | otherwise = isOddCompositeCarmichael n

-- https://oeis.org/A002997
carmichaelNumbers :: (Integral a) => [a]
carmichaelNumbers = filter isOddCompositeCarmichael oddComposites

main :: IO ()
main = do
  print carmichaelNumbers
