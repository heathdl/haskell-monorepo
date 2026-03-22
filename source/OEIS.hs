import Composites (composites, highlyCompositeNumbers, largelyCompositeNumbers, nonprimes)
import Data.Bits (Bits)
import Data.List (intercalate, unfoldr)
import Enumerations (integers)
import NumericalPalindromes (isPalindromeInBase, palindromesInBase)
import Prime (isPrime, primes)
import Prime.Counting (primesLessThan, primesLessThanRoot)
import Sequence.BinaryHammingWeight (binaryHammingWeightWithResidue)
import Sequence.Carmichael (carmichaelNumbers, isCarmichael)
import Sequence.Catalan (catalanNumber, catalanNumbers)
import Sequence.Fermat (fermatNumbers, fermatPrimes)
import Sequence.FreestylePerfect (freestylePerfectNumbers, isFreestylePerfectNumber)
import Sequence.GrayCodes (grayCode, grayCodes)
import Sequence.HappyNumbers (happyNumbers, isHappy, unhappyNumbers)
import Sequence.Mersenne (mersenneExponents, mersenneNumbers, mersennePrimes)
import Sequence.PAddicValuation (valuation, valuationOf)
import Sequence.Palindromic (highlyPalindromicNumbers, isStrictlyNonPalindromicNumber, strictlyNonPalindromicNumbers)
import Sequence.Perfect (isPerfectNumber, perfectNumbers)
import Sequence.PerfectPowers (isPerfectPower, perfectPowers)
import SparselySmoothNumbers (sparselySmoothNumbers)

oeisTrim :: (Show a) => [a] -> [a]
oeisTrim = go 260
  where
    go _ [] = []
    go remaining (x : xs)
      | len > remaining = []
      | otherwise = x : go (remaining - len) xs
      where
        len = length (show x) + 2

oeisShow :: (Show a) => [a] -> String
oeisShow = intercalate ", " . map show . oeisTrim

oeisPrint :: (Show a) => [a] -> IO ()
oeisPrint = putStrLn . oeisShow

-- The prime numbers - https://oeis.org/A000040
a000040_list = primes

is_a000040 = isPrime

-- The composite numbers - https://oeis.org/A002808
a002808_list = composites

-- The non-prime numbers - https://oeis.org/A018252
a018252_list = nonprimes

-- Highly composite numbers - https://oeis.org/A002182
a002182_list = highlyCompositeNumbers

-- Largely composite numbers - https://oeis.org/A067128
a067128_list = largelyCompositeNumbers

-- Evil numbers - https://oeis.org/A001969
a001969_list = binaryHammingWeightWithResidue 2 0

-- Odious numbers - https://oeis.org/A000069
a000069_list = binaryHammingWeightWithResidue 2 1

-- Carmichael numbers - https://oeis.org/A002997
a002997_list = carmichaelNumbers

is_a002997 = isCarmichael

-- Catalan numbers - https://oeis.org/A000108
a000108_list = catalanNumbers

a000108 = catalanNumber

-- Fermat numbers - https://oeis.org/A000051
a000051_list = fermatNumbers

-- Fermat primes - https://oeis.org/A019434
a019434_list = fermatPrimes

-- Freestyle perfect numbers - https://oeis.org/A058007
a058007_list = freestylePerfectNumbers

is_a058007 = isFreestylePerfectNumber

-- Gray codes - https://oeis.org/A003188
a003188_list = grayCodes

a003188 :: (Integral a, Bits a) => a -> a
a003188 = grayCode

-- Happy Numbers - https://oeis.org/A007770
a007770_list = happyNumbers

is_a007770 = isHappy

-- Unhappy Numbers - https://oeis.org/A031177
a031177_list = unhappyNumbers

is_a031177 = not . isHappy

-- Mersenne numbers - https://oeis.org/A001348
a001348_list = map fst mersenneNumbers

-- Mersenne primes - https://oeis.org/A000668
a000668_list = mersennePrimes

-- Mersenne exponents - https://oeis.org/A000043
a000043_list = mersenneExponents

-- 2-adic valuation of n - https://oeis.org/A007814
a007814_list = valuation 2

a007814 = valuationOf 2

-- 3-adic valuation of n - https://oeis.org/A007949
a007949_list = valuation 3

a007949 = valuationOf 3

-- 5-adic valuation of n - https://oeis.org/A112765
a112765_list = valuation 5

a112765 = valuationOf 5

-- Strictly non-palindromic numbers - https://oeis.org/A016038
a016038_list = strictlyNonPalindromicNumbers

is_a016038 = isStrictlyNonPalindromicNumber

-- Highly palindromic numbers - https://oeis.org/A107129
a107129_list = highlyPalindromicNumbers

-- Perfect numbers - https://oeis.org/A000396
a000396_list = perfectNumbers

is_a000396 = isPerfectNumber

-- Perfect powers - https://oeis.org/A001597
a001597_list = perfectPowers

is_a001597 = isPerfectPower

-- pi(n), the number of primes <= n - https://oeis.org/A000720
a000720_list = primesLessThan

-- Number of squared primes not exceeding n. - https://oeis.org/A056811
a056811_list = primesLessThanRoot

-- Binary palindromes - https://oeis.org/A006995
a006995_list = palindromesInBase 2

is_a006995 = isPalindromeInBase 2

-- Trinary palindromes - https://oeis.org/A014190
a014190_list = palindromesInBase 3

is_a014190 = isPalindromeInBase 3

-- Quarternary palindromes - https://oeis.org/A014192
a014192_list = palindromesInBase 4

is_a014192 = isPalindromeInBase 4

-- Quinary palindromes - https://oeis.org/A029952
a029952_list = palindromesInBase 5

is_a029952 = isPalindromeInBase 5

-- Senary palindromes - https://oeis.org/A029953
a029953_list = palindromesInBase 6

is_a029953 = isPalindromeInBase 6

-- Septenary palindromes - https://oeis.org/A029954
a029954_list = palindromesInBase 7

is_a029954 = isPalindromeInBase 7

-- Octal palindromes - https://oeis.org/A029803
a029803_list = palindromesInBase 8

is_a029803 = isPalindromeInBase 8

-- Nonary palindromes - https://oeis.org/A029955
a029955_list = palindromesInBase 9

is_a029955 = isPalindromeInBase 9

-- Decimal palindromes - https://oeis.org/A002113
a002113_list = palindromesInBase 10

is_a002113 = isPalindromeInBase 0

-- 3-smooth numbers - https://oeis.org/A003586
a003586_list = sparselySmoothNumbers [2, 3]

-- 5-smooth numbers - https://oeis.org/A051037
a051037_list = sparselySmoothNumbers [2, 3, 5]

-- 7-smooth numbers - https://oeis.org/A002473
a002473_list = sparselySmoothNumbers [2, 3, 5, 7]

-- 11-smooth numbers - https://oeis.org/A051038
a051038_list = sparselySmoothNumbers [2, 3, 5, 7, 11]

main :: IO ()
main =
  mapM_
    (putStr . showPair)
    [ ("The prime numbers", a000040_list),
      ("The composite numbers", a002808_list),
      ("The non-prime numbers", a018252_list),
      ("Largely composite numbers", a067128_list),
      ("Evil numbers", a001969_list),
      ("Odious numbers", a000069_list),
      ("Catalan numbers", a000108_list),
      ("Fermat numbers", a000051_list),
      ("Fermat primes", a019434_list),
      ("Gray codes", a003188_list),
      ("Mersenne numbers", a001348_list),
      ("Mersenne primes", a000668_list),
      ("2-adic valuation of n", a007814_list),
      ("3-adic valuation of n", a007949_list),
      ("5-adic valuation of n", a112765_list),
      ("Strictly non-palindromic numbers", a016038_list),
      ("Perfect numbers", a000396_list),
      ("Perfect powers", a001597_list),
      ("pi(n), the number of primes <= n", a000720_list),
      ("Number of squared primes not exceeding n.", a056811_list),
      ("Binary palindromes", a006995_list),
      ("Trinary palindromes", a014190_list),
      ("Quarternary palindromes", a014192_list),
      ("Quinary palindromes", a029952_list),
      ("Senary palindromes", a029953_list),
      ("Septenary palindromes", a029954_list),
      ("Octal palindromes", a029803_list),
      ("Nonary palindromes", a029955_list),
      ("Decimal palindromes", a002113_list),
      ("3-smooth numbers", a003586_list),
      ("5-smooth numbers", a051037_list),
      ("7-smooth numbers", a002473_list),
      ("7-smooth numbers", a051038_list),
      -- Slow Sequences
      ("Highly composite numbers", a002182_list),
      ("Carmichael numbers", a002997_list),
      ("Freestyle perfect numbers", a058007_list),
      ("Mersenne exponents", a000043_list),
      ("Highly palindromic numbers", a107129_list)
      -- These use `Int`s, so they break the other sequences.
      -- ("Happy Numbers", a007770_list),
      -- ("Unhappy Numbers", a031177_list),
    ]
  where
    showPair :: (Show a) => (String, [a]) -> String
    showPair (n, s) = n ++ "\n" ++ oeisShow s ++ "\n\n"