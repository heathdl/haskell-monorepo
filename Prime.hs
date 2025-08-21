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
    && value `mod` 3 /= 0
    && go 5 2
  where
    go check increment
      | check * check > value = True
      | value `mod` check == 0 = False
      | otherwise = go (check + increment) (6 - increment)

primeDivisors :: Int -> [Int]
primeDivisors value
  | value < 2 = []
  | otherwise = factor value primes
  where
    factor 1 _ = []
    factor value (divisor : divisors)
      | divisor * divisor > value = [value]
      | value `mod` divisor == 0 = divisor : factor (divide value divisor) (divisor : divisors)
      | otherwise = factor value divisors
    divide value divisor
      | value `mod` divisor == 0 = divide (value `div` divisor) divisor
      | otherwise = value

primeFactorisation :: Int -> [Int]
primeFactorisation value
  | value < 2 = []
  | otherwise = factor value primes
  where
    factor 1 _ = []
    factor value (divisor : divisors)
      | divisor * divisor > value = [value]
      | value `mod` divisor == 0 = divisor : factor (value `div` divisor) (divisor : divisors)
      | otherwise = factor value divisors

main :: IO ()
main = do
  print (take 100 primes)
  print (primeDivisors value)
  print (primeFactorisation value)
  where
    value = 39342594728712960