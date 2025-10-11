module Sequence.RamanujanPi where

import Data.Bits (Bits (shiftL))
import Data.Ratio (Ratio, denominator, numerator, (%))
import Data.Word (Word8)

ramanujanPi :: [Rational]
ramanujanPi = zipWith (\x root -> (9801 * root) / (4 * x)) ramanujanSum root2
  where
    ramanujanFactorialTerm = map (\(a, _, _, _) -> a) (iterate go (1, 1, 1, 1))
      where
        go :: (Integer, Integer, Integer, Integer) -> (Integer, Integer, Integer, Integer)
        go (x, n, n2, n3) = (term, n', n2', n3')
          where
            n' = n + 1
            n2' = n2 + (n `shiftL` 1) + 1
            n3' = n3 + 3 * (n + n2) + 1

            doubN = n `shiftL` 1
            quadN = n `shiftL` 2
            term = ((quadN - 3) * (doubN - 1) * (quadN - 1) * x `shiftL` 3) `div` n3

    ramanujanNumerator = iterate (+ 26390) 1103
    ramanujanDenominator = iterate (* 24591257856) 1

    ramanujanSumTerms = zipWith3 (\a b c -> (a * b) % c) ramanujanFactorialTerm ramanujanNumerator ramanujanDenominator
    ramanujanSum = scanl1 (+) ramanujanSumTerms

    root2 = map (uncurry (%)) (iterate (iterateN 16 next) (1, 1))
      where
        next (p, q) = (p + 2 * q, p + q)
        iterateN 0 f x = x
        iterateN n f x = iterateN (n - 1) f (f x)

digitsOfPi :: String
digitsOfPi = go ramanujanPi 0
  where
    go (a : b : ps) d = concatMap show newDigits ++ go (b : ps) (d + newCount)
      where
        (newCount, newDigits) = countEquality decimalExpansionA decimalExpansionB
        decimalExpansionA = ratioDecimalExpansion a d
        decimalExpansionB = ratioDecimalExpansion b d

    countEquality = go 0
      where
        go n (a : as) (b : bs)
          | a == b = (n', a : subsequent)
          | otherwise = (n, [])
          where
            (n', subsequent) = go (n + 1) as bs

    ratioDecimalExpansion :: Rational -> Int -> [Word8]
    ratioDecimalExpansion r = go rN rD
      where
        rD = denominator r
        rN = numerator r `rem` rD

        go 0 _ _ = []
        go n d 0 = fromIntegral q : go n' d 0
          where
            (q, n') = (10 * n) `quotRem` d
        go n d k = go n' d (k - 1)
          where
            n' = (10 * n) `rem` d

main :: IO ()
main = putStrLn (take 40000 digitsOfPi)

--   mapM_ (\x -> putStrLn ("\\frac{" ++ show (numerator x) ++ "}{" ++ show (denominator x) ++ "}")) (take 16 ramanujanPi)