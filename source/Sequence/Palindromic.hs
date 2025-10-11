module Sequence.Palindromic where

import Digits (digitsFromIntegral)
import NumericalPalindromes (getPalindromicBasesOf)
import Prime (primes)
import ScriptedNumbers (toSubscript)

-- https://oeis.org/A016038
strictlyNonPalindromicNumbers :: (Integral a) => [a]
strictlyNonPalindromicNumbers = 0 : 1 : 2 : 3 : 4 : 6 : filter (null . tail . getPalindromicBasesOf) (drop 2 primes)

-- https://oeis.org/A107129
highlyPalindromicNumbers :: (Integral a) => [a]
highlyPalindromicNumbers = 3 : 5 : 10 : 21 : 36 : 60 : 80 : highlyPalindromicNumbers [120, 132 ..] 6
  where
    highlyPalindromicNumbers (x : xs) currentHighest
      | factorCount > currentHighest = x : highlyPalindromicNumbers xs factorCount
      | otherwise = highlyPalindromicNumbers xs currentHighest
      where
        bases = getPalindromicBasesOf x
        factorCount = length bases

main :: IO ()
main = do
  mapM_ (\x -> putStrLn (show x ++ " = " ++ displayPalindromicForms x)) highlyPalindromicNumbers
  where
    displayPalindromicForms x = take (length str - 1) str
      where
        str = concatMap (displayInBase x) (getPalindromicBasesOf x)
    displayInBase x b = show (digitsFromIntegral b x) ++ toSubscript b ++ " "
