module NumericalPalindromes (digitwiseNextHighestPalindrome, nextHighestPalindrome, palindromesInBase, nthPalindromeInBase, indexOfPalindromeInBase, isPalindromeInBase, getPalindromicBasesOf, numberPalindromicBasePairs) where

import Digits (DigitalRepresentation (DR), countDigits, digitsFromIntegral, digitsToIntegral, reversedDigitsToIntegral)

incrementPrefix :: (Foldable t, Eq a, Num a) => a -> t a -> ([a], Bool)
incrementPrefix base = foldr step ([], True)
  where
    step x (acc, carry)
      | carry && x == base - 1 = (0 : acc, True)
      | carry = ((x + 1) : acc, False)
      | otherwise = (x : acc, False)

digitwiseNextHighestPalindrome :: Int -> DigitalRepresentation -> DigitalRepresentation
digitwiseNextHighestPalindrome base (DR (digits, count))
  | all (== fromIntegral base - 1) digits = DR (1 : replicate (count - 1) 0 ++ [1], count + 1)
  | leftValue <= rightValue = DR (mirrorPrefix prefix', count')
  | otherwise = DR (mirrorPrefix prefix, count)
  where
    (midpoint, parity) = count `quotRem` 2
    prefixLength = midpoint + parity

    left = take midpoint digits
    right = drop (count - prefixLength) digits
    prefix = take prefixLength digits

    leftValue = digitsToIntegral base (DR (left, midpoint))
    rightValue = reversedDigitsToIntegral base (DR (right, prefixLength))

    (prefix', carry) = incrementPrefix base prefix
    count'
      | carry = count + 1
      | otherwise = count

    mirrorPrefix xs = xs ++ reverse (take midpoint xs)

nextHighestPalindrome :: (Integral a) => Int -> a -> a
nextHighestPalindrome base x = digitsToIntegral base digits'
  where
    digits' = digitwiseNextHighestPalindrome base digits
    digits = digitsFromIntegral base x

palindromesInBase :: (Integral a) => Int -> [a]
palindromesInBase base = map (digitsToIntegral base) (iterate next (DR ([0], 1)))
  where
    next (DR (digits, count))
      | all (== base - 1) digits = DR (1 : replicate (count - 1) 0 ++ [1], count + 1)
      | otherwise = DR (digits', count)
      where
        (midpoint, parity) = count `quotRem` 2
        prefix = take (midpoint + parity) digits

        prefix' = fst (incrementPrefix base prefix)
        digits' = prefix' ++ reverse (take midpoint prefix')

nthPalindromeInBase :: (Integral a) => Int -> a -> a
nthPalindromeInBase base value = c * a + digitsToIntegral base digits'
  where
    DR (digits, count) = digitsFromIntegral base c

    x = fromIntegral base ^ countDigits base (value `div` 2)
    y = fromIntegral base * x

    a
      | x + y > value = x
      | otherwise = y
    digits'
      | value <= fromIntegral base = DR ([0], 1)
      | value < x + y = DR (drop 1 (reverse digits), count - 1)
      | otherwise = DR (reverse digits, count)
    c = value - a

indexOfPalindromeInBase :: (Integral a) => Int -> a -> Maybe a
indexOfPalindromeInBase base p
  | base < 2 = Nothing
  | p < 0 = Nothing
  | not (isPalindromeInBase base p) = Nothing
  | otherwise = Just (firstHalfValue + fromIntegral offset)
  where
    DR (digits, count) = digitsFromIntegral base p
    halfCount = (count + 1) `div` 2

    firstHalf = DR (take halfCount digits, halfCount)
    firstHalfValue = digitsToIntegral base firstHalf

    offset
      | count == 1 = 1
      | otherwise = base ^ p * (1 + n * (base - 1))
      where
        (p, n) = (count - 1) `quotRem` 2

isPalindromeInBase :: (Integral a) => Int -> a -> Bool
isPalindromeInBase base value = digits == reverse digits
  where
    DR (digits, _) = digitsFromIntegral base value

getPalindromicBasesOf :: (Integral a) => a -> [Int]
getPalindromicBasesOf value = filter (`isPalindromeInBase` value) [2 .. fromIntegral value - 1]

numberPalindromicBasePairs :: (Integral a) => [(a, [Int])]
numberPalindromicBasePairs = zip [1 ..] (map getPalindromicBasesOf [1 ..])

main :: IO ()
main = do
  print numberPalindromicBasePairs
