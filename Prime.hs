module Prime (primes, isPrime, isPrimeTrialDivision, primeFactorisation, primeFactorMultiset, primeDivisors, eulersTotient) where

import MillerRabin (deterministicMillerRabinPrimalityTest)

primes :: [Int]
primes = 2 : 3 : filter deterministicMillerRabinPrimalityTest [5, 7 ..]

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime 3 = True
isPrime value = all (failsToDivide value) (takeWhile (isLessThanSquareRootOf value) primes)
  where
    failsToDivide x y = x `mod` y /= 0
    isLessThanSquareRootOf x y = y * y <= x

isPrimeTrialDivision :: Int -> Bool
isPrimeTrialDivision 1 = False
isPrimeTrialDivision 2 = True
isPrimeTrialDivision 3 = True
isPrimeTrialDivision value =
  odd value
    && value `mod` 3 /= 0
    && go 5 2
  where
    go check increment
      | check * check > value = True
      | value `mod` check == 0 = False
      | otherwise = go (check + increment) (6 - increment)

primeFactorisation :: Int -> [(Int, Int)]
primeFactorisation value
  | value < 2 = []
  | value < 3 = [(value, 1)]
  | otherwise = go value primes
  where
    go :: Int -> [Int] -> [(Int, Int)]
    go 1 _ = []
    go value (divisor : divisors)
      | divisor * divisor > value = [(value, 1)]
      | value `mod` divisor == 0 = (divisor, termCount) : go remainder divisors
      | otherwise = go value divisors
      where
        (termCount, remainder) = divide divisor value 0
    divide divisor value count
      | remainder == 0 = divide divisor quotient (count + 1)
      | otherwise = (count, value)
      where
        (quotient, remainder) = value `quotRem` divisor

primeFactorMultiset :: Int -> [Int]
primeFactorMultiset value = concatMap (uncurry (flip replicate)) (primeFactorisation value)

primeDivisors :: Int -> [Int]
primeDivisors value = map fst (primeFactorisation value)

eulersTotient :: Int -> Int
eulersTotient value
  | value < 1 = 0
  | otherwise = product [(factor - 1) * factor ^ (count - 1) | (factor, count) <- primeFactorisation value]

main :: IO ()
main = do
  print (primeDivisors value)
  print (primeFactorisation value)
  print (primeFactorMultiset value)
  print (eulersTotient value)
  where
    value = 3 * 3 * 5 * 11