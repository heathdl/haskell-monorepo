module Sequence.PerfectPowers where

import Merge (mergeStrictlyIncreasing)

perfectPowers :: (Integral a) => [a]
perfectPowers = mergeStrictlyIncreasing powers

isPerfectPower :: (Integral a) => a -> Bool
isPerfectPower n
  | n < 4 = False
  | otherwise = any ((== 1) . divideOutOf n) bases
  where
    bases = takeWhile (\b -> b * b <= n) [2 ..]
    divideOutOf n a
      | n `mod` a == 0 = divideOutOf (n `div` a) a
      | otherwise = n

powers :: (Integral a) => [[a]]
powers = [iterate (* k) (k * k) | k <- [2 ..]]

main :: IO ()
main =
  mapM_ (\(a, b) -> putStrLn (show a ++ " " ++ show b)) (zip [1 ..] smallPerfectPowers)
  where
    smallPerfectPowers = takeWhile (<= 100) perfectPowers
