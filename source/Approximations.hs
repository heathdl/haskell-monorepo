module Approximations (digitsOf, formattedDigitsOf) where

import Data.Ratio (denominator, numerator)

digitsOf :: [Rational] -> String
digitsOf = go 0
  where
    go _ [] = []
    go _ [_] = []
    go d (a : b : ps) = concatMap show newDigits ++ go (d + newCount) (b : ps)
      where
        (newCount, newDigits) = extractEqual decimalExpansionA decimalExpansionB
        decimalExpansionA = ratioDecimalExpansion a d
        decimalExpansionB = ratioDecimalExpansion b d

    extractEqual = go 0
      where
        go n (a : as) (b : bs)
          | a == b = (n', a : subsequent)
          | otherwise = (n, [])
          where
            (n', subsequent) = go (n + 1) as bs

    ratioDecimalExpansion r = go rN rD
      where
        rD = denominator r
        rN = numerator r `rem` rD

        go 0 _ _ = [0 .. 0]
        go n d 0 = fromIntegral q : go n' d 0
          where
            (q, n') = (10 * n) `quotRem` d
        go n d k = go n' d (k - 1)
          where
            n' = (10 * n) `rem` d

formattedDigitsOf :: [Rational] -> String
formattedDigitsOf = unlines . map (unwords . chunkEvery 10) . chunkEvery 100 . digitsOf
  where
    chunkEvery _ [] = []
    chunkEvery n xs = take n xs : chunkEvery n (drop n xs)
