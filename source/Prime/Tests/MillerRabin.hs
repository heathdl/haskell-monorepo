module Prime.Tests.MillerRabin
  ( millerRabinDecompose,
    millerRabinWitnessGivenDecomposition,
    millerRabinWitness,
    deterministicMillerRabinPrimalityTest,
    millerRabinPrimalityTest,
    millarRabinPrimeGenerator,
    millerRabinNextPrimeHigherThan,
  )
where

import RaiseToPowerModulo (raiseToSomePowerModulo)
import System.Random (Random (randomR), RandomGen)

millerRabinDecompose :: (Integral a) => a -> (Int, a)
millerRabinDecompose value = go (value - 1) 0
  where
    go excess leadingZeros
      | even excess = go (excess `div` 2) (leadingZeros + 1)
      | otherwise = (leadingZeros, excess)

millerRabinWitnessGivenDecomposition :: (Integral a) => a -> (Int, a) -> a -> Bool
millerRabinWitnessGivenDecomposition value (s, d) base
  | value < 2 = False
  | value == 2 = True
  | even value = False
  | otherwise = checkWitness (raiseToSomePowerModulo base d value) s
  where
    x = raiseToSomePowerModulo base d value

    checkWitness x 0 = x == 1
    checkWitness x k
      | y == 1 && x /= 1 && x /= value - 1 = False
      | otherwise = checkWitness y (k - 1)
      where
        y = (x * x) `mod` value

millerRabinWitness :: (Integral a) => a -> a -> Bool
millerRabinWitness value =
  millerRabinWitnessGivenDecomposition
    value
    (millerRabinDecompose value)

-- http://miller-rabin.appspot.com/
millerRabinBases :: (Integral a) => [a]
millerRabinBases = [2, 325, 9375, 28178, 450775, 9780504, 1795265022]

-- These are values that are not prime divisors of at least one of the bases, it is
-- hardcoded as calculating them on startup was slow due to factorisation.
millerRabinExceptions :: (Integral a) => [a]
millerRabinExceptions = [2, 3, 5, 13, 19, 73, 193, 407521, 299210837]

deterministicMillerRabinPrimalityTest :: (Integral a) => a -> Bool
deterministicMillerRabinPrimalityTest value
  | value > 2 ^ 64 = error "input exceeds 2^64, cannot confirm reliablity of 'deterministic' Miller-Rabin test"
  | value `elem` millerRabinExceptions = True
  | otherwise = all (millerRabinWitnessGivenDecomposition value decomposition) millerRabinBases
  where
    decomposition = millerRabinDecompose value

millerRabinPrimalityTest :: (Integral a, Random a, RandomGen g) => a -> Int -> g -> (Bool, g)
millerRabinPrimalityTest _ 0 generator = (True, generator)
millerRabinPrimalityTest value rounds generator
  | value < 2 = (False, generator)
  | value == 2 = (True, generator)
  | even value = (False, generator)
  | otherwise = go rounds generator
  where
    go 0 generator = (True, generator)
    go rounds generator
      | isPossiblyPrime = go (rounds - 1) generator'
      | otherwise = (False, generator')
      where
        isPossiblyPrime = millerRabinWitnessGivenDecomposition value decomposition base
        (base, generator') = randomR (2, value - 2) generator
    decomposition = millerRabinDecompose value

millarRabinPrimeGenerator :: (Integral a, Random a, RandomGen g) => Int -> a -> g -> [a]
millarRabinPrimeGenerator rounds candidate generator =
  map fst (tail (iterate (uncurry (millerRabinNextPrimeHigherThan rounds)) (candidate, generator)))

millerRabinNextPrimeHigherThan :: (Integral a, Random a, RandomGen g) => Int -> a -> g -> (a, g)
millerRabinNextPrimeHigherThan rounds lowerBound generator
  | lowerBound < 2 = (2, generator)
  | lowerBound == 2 = (3, generator)
  | lowerBound < 5 = (5, generator)
  | otherwise = go initialCandidate initialIncrement generator
  where
    residual = lowerBound `mod` 6
    (initialCandidate, initialIncrement) = case residual of
      0 -> (lowerBound + 1, 2)
      1 -> (lowerBound + 4, 2)
      2 -> (lowerBound + 3, 2)
      3 -> (lowerBound + 2, 2)
      4 -> (lowerBound + 1, 2)
      5 -> (lowerBound + 2, 4)

    go candidate increment generator
      | isPrime = (candidate, generator')
      | otherwise = go (candidate + increment) (6 - increment) generator'
      where
        (isPrime, generator') = millerRabinPrimalityTest candidate rounds generator

main :: IO ()
main = do
  print (deterministicMillerRabinPrimalityTest 97)
  print (deterministicMillerRabinPrimalityTest 312)
