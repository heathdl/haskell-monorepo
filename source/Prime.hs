module Prime (primes, isPrime) where

import Data.Bool (Bool)
import Prime.Tests.MillerRabin (deterministicMillerRabinPrimalityTest, millerRabinPrimalityTest)
import Prime.Tests.TrailDivision (internalTrialDivision)
import System.Random (Random, mkStdGen)

isPrimeTrialDivision :: (Integral a) => a -> Bool
isPrimeTrialDivision = internalTrialDivision primes

primes :: (Integral a) => [a]
primes = 2 : 3 : filter isPrime [5, 7 ..]

isPrime :: (Integral a) => a -> Bool
isPrime value
  | value <= 2 ^ 10 = isPrimeTrialDivision value
  | value <= 2 ^ 64 = deterministicMillerRabinPrimalityTest value
  | otherwise = error "primes list only supports deterministic checks"

main :: IO ()
main = do
  print (take 100 primes :: [Int])
  print (isPrime value)
  where
    value = (3 * 3 * 5 * 11) :: Int
