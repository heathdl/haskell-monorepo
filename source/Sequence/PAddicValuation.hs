module Sequence.PAddicValuation (valuation, valuationOf, withoutFactorsOf) where

import Data.List (transpose)

-- Start with [0], recursively define the sequence to be itself with all values incremented by 1 and new 0s interlaced
-- [0]
-- [1] -> [0, 1]
--        [1, 2] -> [0, 1, 0, 2]
--                  [1, 2, 1, 3] -> [0, 1, 0, 2, 0, 1, 0, 3]

valuation :: (Integral a) => a -> [a]
valuation n = sequence
  where
    sequence = zeros ++ concatMap (\x -> x + 1 : zeros) sequence
    zeros = replicate (fromIntegral n - 1) 0

-- Start with [0], duplicate the entire series, increment the last value, and append it to the current series
-- [0] ++ [1] = [0, 1]
--              [0, 1] ++ [0, 2] = [0, 1, 0, 2]
--                                 [0, 1, 0, 2] ++ [0, 1, 0, 3] = [0, 1, 0, 2, 0, 1, 0, 3]

valuation2 :: (Integral a) => a -> [a]
valuation2 n = start ++ go start
  where
    start = replicate (fromIntegral n - 1) 0 ++ [1]
    go xs = xs' ++ go (xs ++ xs')
      where
        xs' = concat (replicate (fromIntegral n - 2) xs) ++ incrementLast xs
    incrementLast [x] = [x + 1]
    incrementLast (x : xs) = x : incrementLast xs

-- Create repeating "masks" for each power, then count leading 1s per column
-- 2 : [0, 1, 0, 1, 0, 1, 0, 1, ...]
-- 4 : [0, 0, 0, 1, 0, 0, 0, 1, ...]
-- 8 : [0, 0, 0, 0, 0, 0, 0, 1, ...]
--     ...
-- v_2 [0, 1, 0, 2, 0, 1, 0, 3, ...]

valuation3 :: (Integral a) => Int -> [a]
valuation3 n = interlace (map (+ 1) (go masks 1 0))
  where
    interlace (x : xs) = replicate (n - 1) 0 ++ [x] ++ interlace xs
    mask p = cycle (replicate (p - 1) 0 ++ [1])
    masks = transpose (map mask (iterate (* n) n))

    go masks p k = group ++ go rest (p * n) (k + 1)
      where
        (chunk, rest) = splitAt p masks
        group = map (fromIntegral . length . takeWhile (/= 0)) chunk

-- For each number, count how many times you can divide the base out
-- [1, 2, 3, 4, 5, 6, 7, 8]
--  0  1  0  1  0  1  0  1
--     0     1     0     1
--           0           1
--                       0
--  0  1  0  2  0  1  0  3

valuation4 :: (Integral a) => a -> [a]
valuation4 p = map (valuationOf p) [1 ..]

valuationOf :: (Integral a) => a -> a -> a
valuationOf p n
  | n `mod` p == 0 = 1 + valuationOf p (n `div` p)
  | otherwise = 0

withoutFactorsOf :: Int -> [Int]
withoutFactorsOf n = zipWith div [1 ..] (map (n ^) (valuation n))

main :: IO ()
main = do
  putStrLn ("n=2 " ++ show (take (2 ^ 6) (valuation 2)))
  putStrLn ("n=3 " ++ show (take (3 ^ 4) (valuation 3)))
  putStrLn ("n=5 " ++ show (take (5 ^ 2) (valuation 5)))