module Prime (primes, isPrime, isPrimeTrialDivision, primeDivisors, primeFactorisation) where

primes :: [Int]
primes = 2 : 3 : filter isPrime [5, 7 ..]

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
    && value `rem` 3 /= 0
    && go 5 2
  where
    go check increment
      | check * check > value = True
      | value `rem` check == 0 = False
      | otherwise = go (check + increment) (6 - increment)

primeDivisors :: Int -> [Int]
primeDivisors value = filter (divides value) (takeWhile (< value) primes)
  where
    divides x y = x `mod` y == 0
    isLessThanSquareRootOf x y = y * y <= x

primeFactorisation :: Int -> [Int]
primeFactorisation 0 = []
primeFactorisation 1 = []
primeFactorisation value
  | not (null divisors) = first : primeFactorisation (value `div` first)
  | otherwise = [value]
  where
    divisors = primeDivisors value
    first = head divisors

main :: IO ()
main = do
  print (take 100 (filter isPrimeTrialDivision [1 ..]))
  print (take 100 primes)
  print (primeFactorisation 21358)