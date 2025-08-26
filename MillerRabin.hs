module MillerRabin
  ( millerRabinDecompose,
    millerRabinWitnessGivenDecomposition,
    millerRabinWitness,
    millerRabinPrimalityTest,
  )
where

import RaiseToPowerModulo (raiseToSomePowerModulo)
import System.Random (randomRIO)

millerRabinDecompose :: Integer -> (Integer, Integer)
millerRabinDecompose value
  | even value = (leadingZeros + 1, excess)
  | otherwise = (0, value)
  where
    (leadingZeros, excess) = millerRabinDecompose (value `div` 2)

millerRabinWitnessGivenDecomposition :: Integer -> (Integer, Integer) -> Integer -> Bool
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

millerRabinWitness :: Integer -> Integer -> Bool
millerRabinWitness value =
  millerRabinWitnessGivenDecomposition
    value
    (millerRabinDecompose (value - 1))

-- http://miller-rabin.appspot.com/
millerRabinBases :: [Integer]
millerRabinBases = [2, 325, 9375, 28178, 450775, 9780504, 1795265022]

-- Can be calulcated through this, but it is slow due to the factorisation.
-- millerRabinExceptions = map head (group (sort (concatMap primeDivisors [2, 325, 9375, 28178, 450775, 9780504, 1795265022])))
millerRabinExceptions :: [Integer]
millerRabinExceptions = [2, 3, 5, 13, 19, 73, 193, 407521, 299210837]

deterministicMillerRabinPrimalityTest :: Integer -> Bool
deterministicMillerRabinPrimalityTest value
  | value > 2 ^ 64 = error "input exceeds 2^64, cannot confirm reliablity of 'deterministic' Miller-Rabin test"
  | value `elem` millerRabinExceptions = True
  | otherwise = all (millerRabinWitnessGivenDecomposition value decomposition) millerRabinBases
  where
    decomposition = millerRabinDecompose (value - 1)

millerRabinPrimalityTest :: Integer -> Integer -> IO Bool
millerRabinPrimalityTest 0 _ = return True
millerRabinPrimalityTest rounds value =
  do
    a <- randomRIO (2, value - 2)
    if millerRabinWitnessGivenDecomposition value decomposition a
      then millerRabinPrimalityTest (rounds - 1) value
      else return False
  where
    decomposition = millerRabinDecompose (value - 1)

main :: IO ()
main = do
  isPrime <- millerRabinPrimalityTest 64 312
  print isPrime
  isPrime <- millerRabinPrimalityTest 64 97
  print isPrime

  print (deterministicMillerRabinPrimalityTest 312)
  print (deterministicMillerRabinPrimalityTest 97)